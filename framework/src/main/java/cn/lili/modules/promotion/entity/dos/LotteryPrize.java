package cn.lili.modules.promotion.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 固定概率奖项；库存耗尽或券不可领时落为谢谢参与，不重新分配概率。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("li_lottery_prize")
public class LotteryPrize extends BaseEntity {
    @Schema(description = "活动ID，对应li_lottery_activity.id")
    private String activityId;
    @Schema(description = "奖品名称，1至100字，抽取后保存快照")
    private String name;
    @Schema(description = "奖项类型：POINTS积分、COUPON平台活动券、THANKS未中奖；发布后不可变更")
    private String type;
    @Schema(description = "概率万分点，须为100至10000且为100的整数倍，对应1至100的正整数百分比，合计10000；首次发布后固定，库存不足不转移概率")
    private Integer weight;
    @Schema(description = "累计奖品库存，中奖项必须大于0，THANKS为0")
    private Integer total;
    @Schema(description = "已成功发放份数，仅抽奖事务递增，THANKS不占库存")
    private Integer issued;
    @Schema(description = "每次中奖发放整数积分，POINTS必须1至1000000，其他类型为0")
    private Long points;
    @Schema(description = "平台活动券模板ID，对应li_coupon.id，仅COUPON填写")
    private String couponId;
    @Schema(description = "展示顺序，从0开始，发布后固定")
    private Integer sortOrder;
}
