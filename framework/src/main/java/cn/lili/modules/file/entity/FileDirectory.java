package cn.lili.modules.file.entity;

import cn.lili.common.security.enums.UserEnums;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

@Data
@TableName("li_file_directory")
@ApiModel(value = "文件目录")
public class FileDirectory extends BaseEntity {

    /**
     * @see UserEnums
     */
    @ApiModelProperty(value = "文件目录类型")
    private String directoryType;
    @ApiModelProperty(value = "拥有者名称")
    private String directoryName;
    @ApiModelProperty(value = "拥有者id")
    private String ownerId;
    @ApiModelProperty(value = "父分类ID")
    private String parentId;

    @ApiModelProperty(value = "层级")
    @NotNull(message = "层级不能为空")
    @Min(value = 0, message = "层级最小为0")
    @Max(value = 2, message = "层级最大为2")
    private Integer level;
}
