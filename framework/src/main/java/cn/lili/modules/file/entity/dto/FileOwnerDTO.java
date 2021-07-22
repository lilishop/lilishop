package cn.lili.modules.file.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 文件查询所属者参数对象
 *
 * @author Chopper
 * @since 2021-02-22 17:20
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class FileOwnerDTO {

    @ApiModelProperty(value = "拥有者id")
    private String ownerId;

    @ApiModelProperty(value = "用户类型")
    private String userEnums;

}
