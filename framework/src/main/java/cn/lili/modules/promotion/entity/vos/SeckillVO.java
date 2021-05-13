package cn.lili.modules.promotion.entity.vos;

import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.DateUtil;
import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.dos.SeckillApply;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.Arrays;
import java.util.Date;
import java.util.List;

/**
 * 限时抢购视图对象
 *
 * @author paulG
 * @date 2020/8/20
 **/
@Data
public class SeckillVO extends Seckill {

    private static final long serialVersionUID = 2891461638257152270L;

    /**
     * @see cn.lili.modules.promotion.entity.enums.SeckillApplyStatusEnum
     */
    @ApiModelProperty(value = "报名状态")
    private String seckillApplyStatus;

    /**
     * 当前限时抢购下所有的秒杀申请信息
     */
    private List<SeckillApply> seckillApplyList;

    /**
     * 检查当前时间
     */
    public void checkTime() {
        String[] timeRange = this.getHours().split(",");
        int[] hoursSored = Arrays.stream(timeRange).mapToInt(Integer::parseInt).toArray();
        Arrays.sort(hoursSored);
        int lastTime = 0;
        for (int s : hoursSored) {
            if (lastTime == s) {
                throw new ServiceException("抢购区间的值不能重复");
            } else {
                lastTime = s;
            }

            if (s < 0 || s > 23) {
                throw new ServiceException("抢购区间必须在0点到23点的整点时刻");
            }
        }

        // 活动开始时间
        long startTime = this.getStartTime().getTime() / 1000;
        // 报名截止时间
        long applyEndTime = this.getApplyEndTime().getTime() / 1000;

        int timeHour = hoursSored[0];
        int endTimeHour = hoursSored[hoursSored.length - 1];
        //获取活动开始当天0点的时间
        String startDate = DateUtil.toString(startTime, DateUtil.STANDARD_DATE_FORMAT) + " " + (timeHour > 10 ? timeHour : "0" + timeHour) + ":00:00";
        long startDayTime = cn.hutool.core.date.DateUtil.parse(startDate, DateUtil.STANDARD_FORMAT).getTime() / 1000;
        // 结束时间
        String endDate = DateUtil.toString(startTime, DateUtil.STANDARD_DATE_FORMAT) + " " + (endTimeHour > 10 ? endTimeHour : "0" + endTimeHour) + ":59:00";
        long endDayTime = cn.hutool.core.date.DateUtil.parse(endDate, DateUtil.STANDARD_FORMAT).getTime() / 1000;
        //活动时间小于当天开始时间
        if (startDayTime < DateUtil.startOfTodDay()) {
            throw new ServiceException("活动时间不能小于当前时间");
        }

        //报名截止时间小于当前时间
        if (applyEndTime < DateUtil.getDateline()) {
            throw new ServiceException("报名截止时间不能小于当前时间");
        }

        //报名截止时间大于活动开始当天的起始时间
        if (applyEndTime >= startDayTime) {
            throw new ServiceException("报名截止时间不能大于等于活动开始时间");
        }

        this.setStartTime(new Date(startDayTime * 1000));
        this.setEndTime(new Date(endDayTime * 1000));
    }

}
