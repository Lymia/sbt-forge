/*
File derived from ForgeGradle code, available under the terms of the LGPL v2.1
License available at <LICENSE.ForgeGradle>

Original file: https://github.com/MinecraftForge/ForgeGradle/blob/d87353282d468d7655b4e69c621c6ac06e0ca554/src/main/java/net/minecraftforge/gradle/tasks/MergeJarsTask.java
Modified by Lymia to remove dependencies on Gradle and Groovy, and to change the class from a task to a plain method.
*/

package moe.lymia.sbt.forge;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.AnnotationNode;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.FieldNode;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.InnerClassNode;

import com.google.common.base.Function;
import com.google.common.base.MoreObjects;
import com.google.common.base.Objects;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.common.io.ByteStreams;

public class ForgeGradleMergeJars
{
    private static final boolean DEBUG = true;

    private static final String SIDE_CLASS = "Lnet/minecraftforge/fml/relauncher/Side;";
    private static final String SIDE_ONLY_CLASS = "Lnet/minecraftforge/fml/relauncher/SideOnly;";

    public void processJar(File clientInFile, File serverInFile, File classesJar, File outFile) throws IOException
    {
        try (ZipFile cInJar = new ZipFile(clientInFile);
             ZipFile sInJar = new ZipFile(serverInFile);
             ZipFile classesInJar = new ZipFile(classesJar);
             ZipOutputStream outJar = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(outFile))))
        {
            // read in the jars, and initalize some variables
            HashSet<String> resources = new HashSet<String>();
            HashMap<String, ZipEntry> cClasses = getClassEntries(cInJar, outJar, resources);
            HashMap<String, ZipEntry> sClasses = getClassEntries(sInJar, outJar, resources);
            HashSet<String> cAdded = new HashSet<String>();

            // start processing
            for (Entry<String, ZipEntry> entry : cClasses.entrySet())
            {
                String name = entry.getKey();
                ZipEntry cEntry = entry.getValue();
                ZipEntry sEntry = sClasses.get(name);

                if (sEntry == null)
                {
                    copyClass(cInJar, cEntry, outJar, true);
                    cAdded.add(name);
                    continue;
                }

                sClasses.remove(name);
                byte[] cData = readEntry(cInJar, entry.getValue());
                byte[] sData = readEntry(sInJar, sEntry);
                byte[] data = processClass(cData, sData);

                ZipEntry newEntry = new ZipEntry(cEntry.getName());
                try {
                    outJar.putNextEntry(newEntry);
                    outJar.write(data);
                } finally {
                    outJar.closeEntry();
                }
                cAdded.add(name);
            }

            for (Entry<String, ZipEntry> entry : sClasses.entrySet())
            {
                if (DEBUG)
                {
                    System.out.println("Copy class s->c : " + entry.getKey());
                }
                copyClass(sInJar, entry.getValue(), outJar, false);
            }

            for (String internalName : new String[] { SIDE_ONLY_CLASS, SIDE_CLASS })
            {
                String name = internalName.replace("/", ".").substring(1, internalName.length() - 1); // FIXME hack
                String eName = name.replace(".", "/");
                String classPath = eName + ".class";
                ZipEntry newEntry = new ZipEntry(classPath);
                if (!cAdded.contains(eName))
                {
                    try {
                        outJar.putNextEntry(newEntry);
                        outJar.write(getClassBytes(classesInJar, name));
                    } finally {
                        outJar.closeEntry();
                    }
                }
            }

        }
        catch (FileNotFoundException e)
        {
            throw new FileNotFoundException("Could not open input/output file: " + e.getMessage());
        }
    }

    private void copyClass(ZipFile inJar, ZipEntry entry, ZipOutputStream outJar, boolean isClientOnly) throws IOException
    {
        ClassReader reader = new ClassReader(readEntry(inJar, entry));
        ClassNode classNode = new ClassNode();

        reader.accept(classNode, 0);

        if (classNode.visibleAnnotations == null)
        {
            classNode.visibleAnnotations = new ArrayList<AnnotationNode>();
        }
        classNode.visibleAnnotations.add(getSideAnn(isClientOnly));

        ClassWriter writer = new ClassWriter(ClassWriter.COMPUTE_MAXS);
        classNode.accept(writer);
        byte[] data = writer.toByteArray();

        ZipEntry newEntry = new ZipEntry(entry.getName());
        if (outJar != null)
        {
            outJar.putNextEntry(newEntry);
            outJar.write(data);
        }
    }

    private byte[] readEntry(ZipFile inFile, ZipEntry entry) throws IOException
    {
        try (InputStream is = inFile.getInputStream(entry))
        {
            return ByteStreams.toByteArray(is);
        }
    }

    private AnnotationNode getSideAnn(boolean isClientOnly)
    {
        AnnotationNode ann = new AnnotationNode(SIDE_ONLY_CLASS);
        ann.values = new ArrayList<Object>();
        ann.values.add("value");
        ann.values.add(new String[] { SIDE_CLASS, isClientOnly ? "CLIENT" : "SERVER" });
        return ann;
    }

    /**
     * @param inFile From which to read classes and resources
     * @param outFile The place to write resources and ignored classes
     * @param resources The registry to add resources to, and to check against.
     * @return HashMap of all the desired Classes and their ZipEntrys
     * @throws IOException
     */
    private HashMap<String, ZipEntry> getClassEntries(ZipFile inFile, ZipOutputStream outFile, HashSet<String> resources) throws IOException
    {
        HashMap<String, ZipEntry> ret = new HashMap<String, ZipEntry>();

        for (ZipEntry entry : Collections.list(inFile.entries()))
        {
            String entryName = entry.getName();
            // Always skip the manifest
            if ("META-INF/MANIFEST.MF".equals(entryName))
            {
                continue;
            }
            if (entry.isDirectory())
            {
                /*
                 * if (!resources.contains(entryName))
                 * {
                 * outFile.putNextEntry(entry);
                 * }
                 */
                continue;
            }

            if (!entryName.endsWith(".class") || entryName.startsWith("."))
            {
                if (!resources.contains(entryName))
                {
                    ZipEntry newEntry = new ZipEntry(entryName);
                    outFile.putNextEntry(newEntry);
                    outFile.write(readEntry(inFile, entry));
                    resources.add(entryName);
                }
            }
            else
            {
                ret.put(entryName.replace(".class", ""), entry);
            }
        }
        return ret;
    }

    private byte[] getClassBytes(ZipFile classesInJar, String name) throws IOException
    {
        String className = name.replace('.', '/').concat(".class");
        return ByteStreams.toByteArray(classesInJar.getInputStream(classesInJar.getEntry(className)));
    }

    public byte[] processClass(byte[] cIn, byte[] sIn)
    {
        ClassNode cClassNode = getClassNode(cIn);
        ClassNode sClassNode = getClassNode(sIn);

        processFields(cClassNode, sClassNode);
        processMethods(cClassNode, sClassNode);
        processInners(cClassNode, sClassNode);

        ClassWriter writer = new ClassWriter(ClassWriter.COMPUTE_MAXS);
        cClassNode.accept(writer);
        return writer.toByteArray();
    }

    private static boolean innerMatches(InnerClassNode o, InnerClassNode o2)
    {
        if (o.innerName == null && o2.innerName != null) return false;
        if (o.innerName != null && !o.innerName.equals(o2.innerName)) return false;
        if (o.name == null && o2.name != null) return false;
        if (o.name != null && !o.name.equals(o2.name)) return false;
        if (o.outerName == null && o2.outerName != null) return false;
        if (o.outerName != null && o.outerName.equals(o2.outerName)) return false;
        return true;
    }
    private static boolean contains(List<InnerClassNode> list, InnerClassNode node)
    {
        for (InnerClassNode n : list)
            if (innerMatches(n, node))
                return true;
        return false;
    }
    private static void processInners(ClassNode cClass, ClassNode sClass)
    {
        List<InnerClassNode> cIners = cClass.innerClasses;
        List<InnerClassNode> sIners = sClass.innerClasses;

        for (InnerClassNode n : cIners)
        {
            if (!contains(sIners, n))
                sIners.add(n);
        }
        for (InnerClassNode n : sIners)
        {
            if (!contains(cIners, n))
                cIners.add(n);
        }
    }

    private ClassNode getClassNode(byte[] data)
    {
        ClassReader reader = new ClassReader(data);
        ClassNode classNode = new ClassNode();
        reader.accept(classNode, 0);
        return classNode;
    }

    private void processFields(ClassNode cClass, ClassNode sClass)
    {
        List<FieldNode> cFields = cClass.fields;
        List<FieldNode> sFields = sClass.fields;

        int serverFieldIdx = 0;
        if (DEBUG)
            System.out.printf("B: Server List: %s\nB: Client List: %s\n", Lists.transform(sFields, FieldName.instance), Lists.transform(cFields, FieldName.instance));
        for (int clientFieldIdx = 0; clientFieldIdx < cFields.size(); clientFieldIdx++)
        {
            FieldNode clientField = cFields.get(clientFieldIdx);
            if (serverFieldIdx < sFields.size())
            {
                FieldNode serverField = sFields.get(serverFieldIdx);
                if (!clientField.name.equals(serverField.name))
                {
                    boolean foundServerField = false;
                    for (int serverFieldSearchIdx = serverFieldIdx + 1; serverFieldSearchIdx < sFields.size(); serverFieldSearchIdx++)
                    {
                        if (clientField.name.equals(sFields.get(serverFieldSearchIdx).name))
                        {
                            foundServerField = true;
                            break;
                        }
                    }
                    // Found a server field match ahead in the list - walk to it and add the missing server fields to the client
                    if (foundServerField)
                    {
                        boolean foundClientField = false;
                        for (int clientFieldSearchIdx = clientFieldIdx + 1; clientFieldSearchIdx < cFields.size(); clientFieldSearchIdx++)
                        {
                            if (serverField.name.equals(cFields.get(clientFieldSearchIdx).name))
                            {
                                foundClientField = true;
                                break;
                            }
                        }
                        if (!foundClientField)
                        {
                            if (serverField.visibleAnnotations == null)
                            {
                                serverField.visibleAnnotations = new ArrayList<AnnotationNode>();
                            }
                            serverField.visibleAnnotations.add(getSideAnn(false));
                            cFields.add(clientFieldIdx, serverField);
                            if (DEBUG)
                                System.out.printf("1. Server List: %s\n1. Client List: %s\nIdx: %d %d\n", Lists.transform(sFields, FieldName.instance), Lists.transform(cFields, FieldName.instance), serverFieldIdx, clientFieldIdx);
                        }
                    }
                    else
                    {
                        if (clientField.visibleAnnotations == null)
                        {
                            clientField.visibleAnnotations = new ArrayList<AnnotationNode>();
                        }
                        clientField.visibleAnnotations.add(getSideAnn(true));
                        sFields.add(serverFieldIdx, clientField);
                        if (DEBUG)
                            System.out.printf("2. Server List: %s\n2. Client List: %s\nIdx: %d %d\n", Lists.transform(sFields, FieldName.instance), Lists.transform(cFields, FieldName.instance), serverFieldIdx, clientFieldIdx);
                    }
                }
            }
            else
            {
                if (clientField.visibleAnnotations == null)
                {
                    clientField.visibleAnnotations = new ArrayList<AnnotationNode>();
                }
                clientField.visibleAnnotations.add(getSideAnn(true));
                sFields.add(serverFieldIdx, clientField);
                if (DEBUG)
                    System.out.printf("3. Server List: %s\n3. Client List: %s\nIdx: %d %d\n", Lists.transform(sFields, FieldName.instance), Lists.transform(cFields, FieldName.instance), serverFieldIdx, clientFieldIdx);
            }
            serverFieldIdx++;
        }
        if (DEBUG)
            System.out.printf("A. Server List: %s\nA. Client List: %s\n", Lists.transform(sFields, FieldName.instance), Lists.transform(cFields, FieldName.instance));
        if (sFields.size() != cFields.size())
        {
            for (int x = cFields.size(); x < sFields.size(); x++)
            {
                FieldNode sF = sFields.get(x);
                if (sF.visibleAnnotations == null)
                {
                    sF.visibleAnnotations = new ArrayList<AnnotationNode>();
                }
                sF.visibleAnnotations.add(getSideAnn(true));
                cFields.add(x++, sF);
            }
        }
        if (DEBUG)
            System.out.printf("E. Server List: %s\nE. Client List: %s\n", Lists.transform(sFields, FieldName.instance), Lists.transform(cFields, FieldName.instance));
    }

    private static class FieldName implements Function<FieldNode, String>
    {
        public static FieldName instance = new FieldName();

        public String apply(FieldNode in)
        {
            return in.name;
        }
    }

    private void processMethods(ClassNode cClass, ClassNode sClass)
    {
        List<MethodNode> cMethods = cClass.methods;
        List<MethodNode> sMethods = sClass.methods;
        LinkedHashSet<MethodWrapper> allMethods = Sets.newLinkedHashSet();

        int cPos = 0;
        int sPos = 0;
        int cLen = cMethods.size();
        int sLen = sMethods.size();
        String clientName = "";
        String lastName = clientName;
        String serverName = "";
        while (cPos < cLen || sPos < sLen)
        {
            do
            {
                if (sPos >= sLen)
                {
                    break;
                }
                MethodNode sM = sMethods.get(sPos);
                serverName = sM.name;
                if (!serverName.equals(lastName) && cPos != cLen)
                {
                    if (DEBUG)
                    {
                        System.out.printf("Server -skip : %s %s %d (%s %d) %d [%s]\n", sClass.name, clientName, cLen - cPos, serverName, sLen - sPos, allMethods.size(), lastName);
                    }
                    break;
                }
                MethodWrapper mw = new MethodWrapper(sM);
                mw.server = true;
                allMethods.add(mw);
                if (DEBUG)
                {
                    System.out.printf("Server *add* : %s %s %d (%s %d) %d [%s]\n", sClass.name, clientName, cLen - cPos, serverName, sLen - sPos, allMethods.size(), lastName);
                }
                sPos++;
            } while (sPos < sLen);
            do
            {
                if (cPos >= cLen)
                {
                    break;
                }
                MethodNode cM = cMethods.get(cPos);
                lastName = clientName;
                clientName = cM.name;
                if (!clientName.equals(lastName) && sPos != sLen)
                {
                    if (DEBUG)
                    {
                        System.out.printf("Client -skip : %s %s %d (%s %d) %d [%s]\n", cClass.name, clientName, cLen - cPos, serverName, sLen - sPos, allMethods.size(), lastName);
                    }
                    break;
                }
                MethodWrapper mw = new MethodWrapper(cM);
                mw.client = true;
                allMethods.add(mw);
                if (DEBUG)
                {
                    System.out.printf("Client *add* : %s %s %d (%s %d) %d [%s]\n", cClass.name, clientName, cLen - cPos, serverName, sLen - sPos, allMethods.size(), lastName);
                }
                cPos++;
            } while (cPos < cLen);
        }

        cMethods.clear();
        sMethods.clear();

        for (MethodWrapper mw : allMethods)
        {
            if (DEBUG)
            {
                System.out.println(mw);
            }
            cMethods.add(mw.node);
            sMethods.add(mw.node);
            if (mw.server && mw.client)
            {
                // no op
            }
            else
            {
                if (mw.node.visibleAnnotations == null)
                {
                    mw.node.visibleAnnotations = Lists.newArrayListWithExpectedSize(1);
                }

                mw.node.visibleAnnotations.add(getSideAnn(mw.client));
            }
        }
    }

    private class MethodWrapper
    {
        private MethodNode node;
        public boolean     client;
        public boolean     server;

        public MethodWrapper(MethodNode node)
        {
            this.node = node;
        }

        @Override
        public boolean equals(Object obj)
        {
            if (obj == null || !(obj instanceof MethodWrapper))
            {
                return false;
            }
            MethodWrapper mw = (MethodWrapper) obj;
            boolean eq = Objects.equal(node.name, mw.node.name) && Objects.equal(node.desc, mw.node.desc);
            if (eq)
            {
                mw.client = client | mw.client;
                mw.server = server | mw.server;
                client = client | mw.client;
                server = server | mw.server;
                if (DEBUG)
                {
                    System.out.printf(" eq: %s %s\n", this, mw);
                }
            }
            return eq;
        }

        @Override
        public int hashCode()
        {
            return Objects.hashCode(node.name, node.desc);
        }

        @Override
        public String toString()
        {
            return MoreObjects.toStringHelper(this).add("name", node.name).add("desc", node.desc).add("server", server).add("client", client).toString();
        }
    }
}
