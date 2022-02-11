package cn.lili.modules.promotion.entity.dos;

import cn.lili.modules.promotion.entity.enums.KanJiaStatusEnum;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;


/**
 * 砍价活动参与实体类
 *
 * @author qiuqiu
 * @since 2020-7-1 10:44 上午
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("li_kanjia_activity")
@ApiModel(value = "砍价活动参与对象")
public class KanjiaActivity extends BaseEntity {


    private static final long serialVersionUID = -1583030890805926292L;

    @ApiModelProperty(value = "砍价商品id")
    private String kanjiaActivityGoodsId;

    @ApiModelProperty(value = "发起砍价活动会员id")
    private String memberId;

    @ApiModelProperty(value = "发起砍价活动会员名称")
    private String memberName;

    @ApiModelProperty(value = "剩余购买金额")
    private Double surplusPrice;

    @ApiModelProperty(value = "砍价最低购买金额")
    private Double purchasePrice;

    @ApiModelProperty(value = "砍价商品skuId")
    private String skuId;

    @ApiModelProperty(value = "货品名称")
    private String goodsName;

    @ApiModelProperty(value = "缩略图")
    private String thumbnail;

    /**
     * @see KanJiaStatusEnum
     */
    @ApiModelProperty(value = "砍价活动状态")
    private String status;


}