package cn.lili.generator;

import cn.hutool.core.util.StrUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.StringUtils;
import cn.lili.generator.bean.Entity;
import org.beetl.core.Configuration;
import org.beetl.core.GroupTemplate;
import org.beetl.core.Template;
import org.beetl.core.resource.ClasspathResourceLoader;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;


/**
 * 代码生成器 Mybatis-Plus
 *
 * @author Chopper
 */
public class CodeGenerator {

    /**
     * 代码生成在哪个项目
     */
    private static final String project = "framework";

    /**
     * 代码生成在哪个包下边
     */
    private static final String packages = "cn.lili.modules.";

    /**
     * modules
     */
    private static final String modules = "message";


    /**
     * 实体类名
     * 建议仅需修改
     */
    private static final String className = "ShortLink";

    /**
     * 类说明描述
     * 建议仅需修改
     */
    private static final String description = "短链接";

    /**
     * 作者名
     * 建议仅需修改
     */
    private static final String author = "Chopper";

    /**
     * 数据库表名前缀
     * 下方请根据需要修改
     */
    private static final String tablePre = "li_";

    /**
     * 主键类型
     */
    private static final String primaryKeyType = "String";


    /**
     * endity
     */
    private static final String entityPackage = packages + modules + ".entity";

    /**
     * dao
     */
    private static final String daoPackage = packages + modules + ".mapper";

    /**
     * service
     */
    private static final String servicePackage = packages + modules + ".service";

    /**
     * serviceImpl
     */
    private static final String serviceImplPackage = packages + modules + ".serviceimpl";

    /**
     * controller
     */
    private static final String controllerPackage = packages + modules + ".controller";

    /**
     * 运行该主函数即可生成代码
     *
     * @param args
     * @throws IOException
     */
    public static void main(String[] args) throws IOException {
        //模板路径
        ClasspathResourceLoader resourceLoader = new ClasspathResourceLoader("/templates/");
        Configuration cfg = Configuration.defaultConfiguration();
        GroupTemplate gt = new GroupTemplate(resourceLoader, cfg);
        //生成代码
        generateCode(gt);
        //根据类名删除生成的代码
//       deleteCode(className);
    }

    /**
     * 生成代码
     *
     * @param gt
     * @throws IOException
     */
    private static void generateCode(GroupTemplate gt) throws IOException {

        Template entityTemplate = gt.getTemplate("entity.btl");
        Template daoTemplate = gt.getTemplate("mapper.btl");
        Template serviceTemplate = gt.getTemplate("service.btl");
        Template serviceImplTemplate = gt.getTemplate("serviceImpl.btl");
        Template controllerTemplate = gt.getTemplate("controller.btl");
        Template mapperXmlTemplate = gt.getTemplate("mapperXml.btl");

        Entity entity = new Entity();
        entity.setEntityPackage(entityPackage);
        entity.setDaoPackage(daoPackage);
        entity.setServicePackage(servicePackage);
        entity.setServiceImplPackage(serviceImplPackage);
        entity.setControllerPackage(controllerPackage);
        entity.setAuthor(author);
        entity.setClassName(className);
        entity.setTableName(tablePre + StringUtils.camel2Underline(className));
        entity.setClassNameLowerCase(name(className, false));
        entity.setDescription(description);
        entity.setPrimaryKeyType(primaryKeyType);

        OutputStream out = null;

        //生成实体类代码
        entityTemplate.binding("entity", entity);
        String entityResult = entityTemplate.render();
        System.out.println(entityResult);
        //创建文件
        String entityFileUrl = System.getProperty("user.dir") + "/" + project + "/src/main/java/" + dotToLine(entityPackage) + "/" + className + ".java";
        File entityFile = new File(entityFileUrl);
        File entityDir = entityFile.getParentFile();
        if (!entityDir.exists()) {
            entityDir.mkdirs();
        }
        if (!entityFile.exists()) {
            //若文件存在则不重新生成
            entityFile.createNewFile();
            out = new FileOutputStream(entityFile);
            entityTemplate.renderTo(out);
        }

        //生成dao代码
        daoTemplate.binding("entity", entity);
        String daoResult = daoTemplate.render();
        System.out.println(daoResult);
        //创建文件
        String daoFileUrl = System.getProperty("user.dir") + "/" + project + "/src/main/java/" + dotToLine(daoPackage) + "/" + className + "Mapper.java";
        File daoFile = new File(daoFileUrl);
        File daoDir = daoFile.getParentFile();
        if (!daoDir.exists()) {
            daoDir.mkdirs();
        }
        if (!daoFile.exists()) {
            //若文件存在则不重新生成
            daoFile.createNewFile();
            out = new FileOutputStream(daoFile);
            daoTemplate.renderTo(out);
        }

        //生成service代码
        serviceTemplate.binding("entity", entity);
        String serviceResult = serviceTemplate.render();
        System.out.println(serviceResult);
        //创建文件
        String serviceFileUrl = System.getProperty("user.dir") + "/" + project + "/src/main/java/" + dotToLine(servicePackage) + "/" + className + "Service.java";
        File serviceFile = new File(serviceFileUrl);
        File serviceDir = serviceFile.getParentFile();
        if (!serviceDir.exists()) {
            serviceDir.mkdirs();
        }
        if (!serviceFile.exists()) {
            //若文件存在则不重新生成
            serviceFile.createNewFile();
            out = new FileOutputStream(serviceFile);
            serviceTemplate.renderTo(out);
        }

        //生成serviceImpl代码
        serviceImplTemplate.binding("entity", entity);
        String serviceImplResult = serviceImplTemplate.render();
        System.out.println(serviceImplResult);
        //创建文件
        System.out.println(System.getProperty("user.dir"));
        String serviceImplFileUrl = System.getProperty("user.dir") + "/" + project + "/src/main/java/" + dotToLine(serviceImplPackage) + "/" + className + "ServiceImpl.java";
        File serviceImplFile = new File(serviceImplFileUrl);
        File serviceImplDir = serviceImplFile.getParentFile();
        if (!serviceImplDir.exists()) {
            serviceImplDir.mkdirs();
        }
        if (!serviceImplFile.exists()) {
            //若文件存在则不重新生成
            serviceImplFile.createNewFile();
            out = new FileOutputStream(serviceImplFile);
            serviceImplTemplate.renderTo(out);
        }

        //生成controller代码
        controllerTemplate.binding("entity", entity);
        String controllerResult = controllerTemplate.render();
        System.out.println(controllerResult);
        //创建文件
        String controllerFileUrl = System.getProperty("user.dir") + "/" + project + "/src/main/java/" + dotToLine(controllerPackage) + "/" + className + "Controller.java";
        File controllerFile = new File(controllerFileUrl);
        File controllerDir = controllerFile.getParentFile();
        if (!controllerDir.exists()) {
            controllerDir.mkdirs();
        }
        if (!controllerFile.exists()) {
            //若文件存在则不重新生成
            controllerFile.createNewFile();
            out = new FileOutputStream(controllerFile);
            controllerTemplate.renderTo(out);
        }

        //生成mapperXml代码
        mapperXmlTemplate.binding("entity", entity);
        String mapperXmlResult = mapperXmlTemplate.render();
        System.out.println(mapperXmlResult);
        //创建文件
        String mapperXmlFileUrl = System.getProperty("user.dir") + "/" + project + "/src/main/resources/mapper/" + className + "Mapper.xml";
        File mapperXmlFile = new File(mapperXmlFileUrl);
        File mapperXmlDir = mapperXmlFile.getParentFile();
        if (!mapperXmlDir.exists()) {
            mapperXmlDir.mkdirs();
        }
        if (!mapperXmlFile.exists()) {
            //若文件存在则不重新生成
            mapperXmlFile.createNewFile();
            out = new FileOutputStream(mapperXmlFile);
            mapperXmlTemplate.renderTo(out);
        }

        if (out != null) {
            out.close();
        }
        System.out.println("生成代码成功！");
    }

    /**
     * 删除指定类代码
     *
     * @param className
     * @throws IOException
     */
    private static void deleteCode(String className) throws IOException {

        String entityFileUrl = System.getProperty("user.dir") + "/" + project + "/src/main/java/" + dotToLine(entityPackage) + "/" + className + ".java";
        File entityFile = new File(entityFileUrl);
        if (entityFile.exists()) {
            entityFile.delete();
        }
        String daoFileUrl = System.getProperty("user.dir") + "/" + project + "/src/main/java/" + dotToLine(daoPackage) + "/" + className + "Mapper.java";
        File daoFile = new File(daoFileUrl);
        if (daoFile.exists()) {
            daoFile.delete();
        }

        String serviceFileUrl = System.getProperty("user.dir") + "/" + project + "/src/main/java/" + dotToLine(servicePackage) + "/" + className + "Service.java";
        File serviceFile = new File(serviceFileUrl);
        if (serviceFile.exists()) {
            serviceFile.delete();
        }

        String serviceImplFileUrl = System.getProperty("user.dir") + "/" + project + "/src/main/java/" + dotToLine(serviceImplPackage) + "/" + className + "ServiceImpl.java";
        File serviceImplFile = new File(serviceImplFileUrl);
        if (serviceImplFile.exists()) {
            serviceImplFile.delete();
        }

        String controllerFileUrl = System.getProperty("user.dir") + "/" + project + "/src/main/java/" + dotToLine(controllerPackage) + "/" + className + "Controller.java";
        File controllerFile = new File(controllerFileUrl);
        if (controllerFile.exists()) {
            controllerFile.delete();
        }

        String mapperXmlFileUrl = System.getProperty("user.dir") + "/" + project + "/src/main/resources/mapper/" + className + "Mapper.xml";
        File mapperXmlFile = new File(mapperXmlFileUrl);
        if (mapperXmlFile.exists()) {
            mapperXmlFile.delete();
        }

        System.out.println("删除代码完毕！");
    }

    /**
     * 点转斜线
     *
     * @param str
     * @return
     */
    public static String dotToLine(String str) {
        return str.replace(".", "/");
    }


    /**
     * 首字母是否大小写
     *
     * @param name
     * @param isFirstUpper
     * @return
     */
    public static String name(String name, boolean isFirstUpper) {

        if (StrUtil.isBlank(name)) {
            throw new ServiceException("name不能为空");
        }

        if (name.length() == 1) {
            if (isFirstUpper) {
                return name.toUpperCase();
            } else {
                return name.toLowerCase();
            }
        }

        StringBuffer sb = new StringBuffer();
        if (isFirstUpper) {
            sb.append(Character.toUpperCase(name.charAt(0)));
        } else {
            sb.append(Character.toLowerCase(name.charAt(0)));
        }
        sb.append(name.substring(1));
        return sb.toString();
    }
}
