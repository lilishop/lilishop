package cn.lili.modules.promotion.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.*;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import java.util.Date;
import java.util.List;

/**
 * 平台每日免费抽奖活动。首次发布后冻结配置，仅允许上下架，已完成记录不受影响。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("li_lottery_activity")
public class LotteryActivity extends BaseEntity {
    @Schema(description = "活动名称，1至100字")
    private String name;
    @Schema(description = "活动规则说明，最多2000字")
    private String description;
    @Schema(description = "每会员每活动北京时间自然日免费次数，1至100次")
    private Integer dailyLimit;
    @Schema(description = "是否上架；true上架、false下架，仍受时间区间约束")
    private Boolean enabled;
    @Schema(description = "是否曾发布；发布后奖池与时间规则冻结，只允许上下架")
    private Boolean published;
    @Schema(description = "开始时间，Asia/Shanghai，含此时刻")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date startTime;
    @Schema(description = "结束时间，Asia/Shanghai，不含此时刻")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date endTime;
    @TableField(exist = false)
    @Schema(description = "有序奖池，2至8项，必须包含一个谢谢参与，概率总和10000万分点")
    private List<LotteryPrize> prizes;
}
