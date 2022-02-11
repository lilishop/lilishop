package cn.lili.modules.statistics.serviceimpl;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.member.entity.dos.MemberEvaluation;
import cn.lili.modules.statistics.mapper.MemberEvaluationStatisticsMapper;
import cn.lili.modules.statistics.service.MemberEvaluationStatisticsService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.Objects;

/**
 * 会员商品评价业务层实现
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
@Service
public class MemberEvaluationStatisticsServiceImpl extends ServiceImpl<MemberEvaluationStatisticsMapper, MemberEvaluation> implements MemberEvaluationStatisticsService {


    @Override
    public long todayMemberEvaluation() {
        return this.count(new LambdaQueryWrapper<MemberEvaluation>().ge(MemberEvaluation::getCreateTime, DateUtil.beginOfDay(new DateTime())));
    }

    @Override
    public long getWaitReplyNum() {
        QueryWrapper<MemberEvaluation> queryWrapper = Wrappers.query();
        queryWrapper.eq(CharSequenceUtil.equals(Objects.requireNonNull(UserContext.getCurrentUser()).getRole().name(), UserEnums.STORE.name()),
                "store_id", UserContext.getCurrentUser().getStoreId());
        queryWrapper.eq("reply_status", false);
        return this.count(queryWrapper);
    }

}