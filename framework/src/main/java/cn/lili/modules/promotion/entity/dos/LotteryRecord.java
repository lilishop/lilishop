package cn.lili.modules.promotion.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import java.time.LocalDate;

/**
 * 已完成抽奖及发奖凭据，包含未中奖；余额、券、库存和次数与本记录同事务提交。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("li_lottery_record")
public class LotteryRecord extends BaseEntity {
    @Schema(description = "活动ID，对应li_lottery_activity.id")
    private String activityId;
    @Schema(description = "活动名称快照")
    private String activityName;
    @Schema(description = "会员ID，对应li_member.id，仅取登录上下文")
    private String memberId;
    @Schema(description = "会员内唯一请求号，网络重试必须复用，最多64位字母数字或横线")
    private String requestId;
    @Schema(description = "次数所属自然日，Asia/Shanghai；未中奖同样消耗一次")
    private LocalDate drawDay;
    @Schema(description = "最终奖项ID，对应li_lottery_prize.id")
    private String prizeId;
    @Schema(description = "最终奖项名称快照")
    private String prizeName;
    @Schema(description = "结果类型快照：POINTS积分、COUPON优惠券、THANKS未中奖")
    private String prizeType;
    @Schema(description = "实际发放整数积分快照，非积分奖为0")
    private Long points;
    @Schema(description = "实际发放会员券ID，对应li_member_coupon.id，其内保存券规则与有效期快照")
    private String memberCouponId;
}
