package cn.lili.modules.promotion.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 砍价活动参与实体类
 *
 * @author qiuqiu
 * @date 2020-7-1 10:44 上午
 */
@Data
@Entity
@Table(name = "li_kanjia_activity")
@TableName("li_kanjia_activity")
@ApiModel(value = "砍价活动参与对象")
public class KanjiaActivity extends BaseEntity {


    private static final long serialVersionUID = -1583030890805926292L;

    @ApiModelProperty(value = "砍价商品id")
    private String kanjiaActivityGoodsId;

    @ApiModelProperty(value = "参与砍价活动会员id")
    private String memberId;

    @ApiModelProperty(value = "参与砍价活动会员名称")
    private String memberName;

    @ApiModelProperty(value = "剩余购买金额")
    private Double surplusPrice;

    @ApiModelProperty(value = "砍价商品skuId")
    private String skuId;

    @ApiModelProperty(value = "货品名称")
    private String goodsName;

    @ApiModelProperty(value = "缩略图")
    private String thumbnail;

    @ApiModelProperty(value = "我的砍价状态")
    private String status;


}