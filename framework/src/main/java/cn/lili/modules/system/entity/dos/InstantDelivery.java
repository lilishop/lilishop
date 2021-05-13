package cn.lili.modules.system.entity.dos;


import cn.lili.base.BaseEntity;
import cn.lili.modules.system.entity.vo.InstantDeliveryVO;
import com.baomidou.mybatisplus.annotation.TableName;
import com.google.gson.Gson;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 即时配送
 *
 * @author pikachu
 * @date 2020/12/01 15:58
 */
@Data
@Entity
@Table(name = "li_instant_delivery")
@TableName("li_instant_delivery")
@ApiModel(value = "即时配送")
@AllArgsConstructor
@NoArgsConstructor
public class InstantDelivery extends BaseEntity {


    /**
     * 即时配送名称
     */
    @ApiModelProperty(value = "即时配送名称")
    private String deliveryName;
    /**
     * 是否开启即时配送,1开启，0未开启
     */
    @ApiModelProperty(value = "是否开启即时配送,1开启，0未开启")
    private Integer deliveryOpen;
    /**
     * 即时配送配置
     */
    @ApiModelProperty(value = "即时配送配置")
    @Column(columnDefinition = "TEXT")
    private String deliveryConfig;
    /**
     * 即时配送bean
     */
    @ApiModelProperty(value = "即时配送bean")
    private String deliveryBean;

    @ApiModelProperty(value = "封面图片")
    private String images;


    /**
     * 根据vo参数构建do
     *
     * @param instantDeliveryVO
     */
    public InstantDelivery(InstantDeliveryVO instantDeliveryVO) {
        this.setDeliveryName(instantDeliveryVO.getDeliveryName());
        this.setDeliveryOpen(instantDeliveryVO.getDeliveryOpen());
        this.setDeliveryBean(instantDeliveryVO.getDeliveryBean());
        Gson gson = new Gson();
        this.setDeliveryConfig(gson.toJson(instantDeliveryVO.getConfigItems()));
    }

}