package cn.lili.modules.system.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

/**
 * 行政地区
 * @author Chopper
 * @since 2020-02-25 14:10:16
 */
@Data
@TableName("li_region")
@ApiModel(value = "行政地区")
public class Region extends BaseEntity {

    private static final long serialVersionUID = 418341656517240988L;

    @NotEmpty(message = "父id不能为空")
    @ApiModelProperty(value = "父id")
    private String parentId;

    @NotEmpty(message = "区域编码不能为空")
    @ApiModelProperty(value = "区域编码")
    private String adCode;

    @ApiModelProperty(value = "城市代码")
    private String cityCode;

    @NotEmpty(message = "区域中心点经纬度不能为空")
    @ApiModelProperty(value = "区域中心点经纬度")
    private String center;

    @ApiModelProperty(value =
            "行政区划级别" +
                    "country:国家" +
                    "province:省份（直辖市会在province和city显示）" +
                    "city:市（直辖市会在province和city显示）" +
                    "district:区县" +
                    "street:街道")
    @NotEmpty(message = "品牌名称不能为空")
    private String level;

    @NotEmpty(message = "名称不能为空")
    @ApiModelProperty(value = "名称")
    private String name;

    @NotEmpty(message = "行政地区路径不能为空")
    @ApiModelProperty(value = "行政地区路径，类似：1，2，3 ")
    private String path;

    @NotNull(message = "排序不能为空")
    @ApiModelProperty(value = "排序")
    private Integer orderNum;

}