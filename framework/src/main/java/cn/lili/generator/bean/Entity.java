package cn.lili.generator.bean;

import lombok.Data;

/**
 * @author Chopper
 */
@Data
public class Entity {

    /**
     * 实体类，dao，service，controller目录
     */
    private String entityPackage;

    private String daoPackage;

    private String servicePackage;

    private String serviceImplPackage;

    private String controllerPackage;


    /**
     * 作者
     */
    private String author;

    /**
     * 类名
     */
    private String className;

    /**
     * 首字母小写的类名字，用于模版内的变量名称
     */
    private String classNameLowerCase;

    /**
     * 数据库
     */
    private String tableName;

    /**
     * 类说明描述,一般设定关键字即可 例如：会员
     */
    private String description;

    /**
     * 主键类型
     */
    private String primaryKeyType;

}
