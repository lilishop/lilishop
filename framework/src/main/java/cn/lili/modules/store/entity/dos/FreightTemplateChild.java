package cn.lili.modules.store.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 运费模板子配置
 *
 * @author Chopper
 * @since 2020/11/17 4:27 下午
 */
@Data
@TableName("li_freight_template_child")
@ApiModel(value = "运费模板子配置")
public class FreightTemplateChild extends BaseEntity {

    private static final long serialVersionUID = -5043707833032504674L;

    @ApiModelProperty(value = "店铺模板ID")
    private String freightTemplateId;

    @ApiModelProperty(value = "首重/首件")
    private Double firstCompany;

    @ApiModelProperty(value = "运费")
    private Double firstPrice;

    @ApiModelProperty(value = "续重/续件")
    private Double continuedCompany;

    @ApiModelProperty(value = "续费")
    private Double continuedPrice;

    @ApiModelProperty(value = "地址，示例参数：上海,江苏,浙江")
    private String area;

    @ApiModelProperty(value = "地区ID，示例参数：1,2,3,4")
    private String areaId;

}
