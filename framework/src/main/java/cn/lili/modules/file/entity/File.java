package cn.lili.modules.file.entity;

import cn.lili.mybatis.BaseEntity;
import cn.lili.modules.file.entity.enums.FileCategoryEnum;
import com.baomidou.mybatisplus.annotation.TableName;
import tools.jackson.databind.annotation.JsonSerialize;
import tools.jackson.databind.ser.std.ToStringSerializer;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;


/**
 * 文件系统
 *
 * @author Chopper
 * @since 2020/11/26 15:35
 */
@Data
@TableName("li_file")
@Schema(description = "文件")
@EqualsAndHashCode(callSuper = false)
public class File extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "原文件名")
    private String name;

    @Schema(description = "存储文件名")
    private String fileKey;

    @Schema(description = "大小")
    @JsonSerialize(using = ToStringSerializer.class)
    private Long fileSize;

    @Schema(description = "文件类型")
    private String fileType;

    @Schema(description = "文件分类")
    /**
     * @see FileCategoryEnum
     */
    private String fileCategory;

    @Schema(description = "路径")
    private String url;

    @Schema(description = "拥有者id")
    private String ownerId;

    @Schema(description = "拥有者名称")
    private String ownerName;

    @Schema(description = "用户类型")
    private String userEnums;

    @Schema(description = "文件夹ID")
    private String fileDirectoryId;
}