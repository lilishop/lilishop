package cn.lili.modules.statistics.service;

import cn.lili.modules.member.entity.dos.MemberEvaluation;
import cn.lili.modules.member.entity.dto.EvaluationQueryParams;
import cn.lili.modules.member.entity.dto.MemberEvaluationDTO;
import cn.lili.modules.member.entity.vo.EvaluationNumberVO;
import cn.lili.modules.member.entity.vo.MemberEvaluationListVO;
import cn.lili.modules.member.entity.vo.MemberEvaluationVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 会员商品评价统计
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
public interface MemberEvaluationStatisticsService extends IService<MemberEvaluation> {

    /**
     * 获取今天新增的评价数量
     *
     * @return 今日评价数量
     */
    Integer todayMemberEvaluation();

    /**
     * 获取等待回复评价数量
     *
     * @return 等待回复评价数量
     */
    Integer getWaitReplyNum();

}