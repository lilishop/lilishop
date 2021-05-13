package cn.lili.modules.store.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 运费模板子配置
 *
 * @author Chopper
 * @date 2020/11/17 4:27 下午
 */
@Data
@Entity
@Table(name = "li_freight_template_child")
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
    @Column(columnDefinition = "TEXT")
    private String area;

    @ApiModelProperty(value = "地区ID，示例参数：1,2,3,4")
    @Column(columnDefinition = "TEXT")
    private String areaId;

}
