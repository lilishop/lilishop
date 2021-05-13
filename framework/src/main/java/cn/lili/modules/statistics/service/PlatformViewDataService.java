package cn.lili.modules.statistics.service;

import cn.lili.modules.member.entity.vo.MemberDistributionVO;
import cn.lili.modules.statistics.model.dos.PlatformViewData;
import cn.lili.modules.statistics.model.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.model.vo.OnlineMemberVO;
import cn.lili.modules.statistics.model.vo.PlatformViewVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 平台PV统计
 *
 * @author Bulbasaur
 * @date 2020/12/9 11:06
 */
public interface PlatformViewDataService extends IService<PlatformViewData> {


    /**
     * 当前在线人数
     *
     * @return
     */
    Long online();

    /**
     * 会员分布
     *
     * @return
     */
    List<MemberDistributionVO> memberDistribution();

    /**
     * 在线人数记录
     *
     * @return
     */
    List<OnlineMemberVO> statisticsOnline();

    /**
     * 数据查询
     *
     * @param queryParam
     * @return
     */
    List<PlatformViewVO> list(StatisticsQueryParam queryParam);

    /**
     * 查询累计访客数
     *
     * @param queryParam
     * @return
     */
    Integer countUv(StatisticsQueryParam queryParam);
}