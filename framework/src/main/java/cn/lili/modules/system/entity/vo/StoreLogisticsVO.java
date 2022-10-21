package cn.lili.modules.system.entity.vo;

import cn.lili.mybatis.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 物流公司设置
 *
 * @author Chopper
 * @since 2020/11/17 8:01 下午
 */

@Data
@ApiModel(value = "物流公司VO")
public class StoreLogisticsVO extends BaseEntity {

    @ApiModelProperty(value = "物流公司ID")
    private String logisticsId;

    @ApiModelProperty(value = "物流公司名称")
    private String name;

    @ApiModelProperty(value = "已选择", notes = "如果已选择则有值，没有选择则无值")
    private String selected;

    @ApiModelProperty(value = "是否使用电子面单")
    private Boolean faceSheetFlag;
}