package cn.lili.modules.member.serviceimpl;


import cn.lili.common.utils.PageUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.MemberPointsHistory;
import cn.lili.modules.member.entity.vo.MemberPointsHistoryVO;
import cn.lili.modules.member.mapper.MemberPointsHistoryMapper;
import cn.lili.modules.member.service.MemberPointsHistoryService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 会员积分历史业务层实现
 *
 * @author Bulbasaur
 * @date 2020-02-25 14:10:16
 */
@Service
public class MemberPointsHistoryServiceImpl extends ServiceImpl<MemberPointsHistoryMapper, MemberPointsHistory> implements MemberPointsHistoryService {

    @Override
    public MemberPointsHistoryVO getMemberPointsHistoryVO(String memberId) {
        MemberPointsHistoryVO memberPointsHistoryVO = new MemberPointsHistoryVO();
        Long point = 0L;
        Long variablePoint = 0L;

        if (StringUtils.isNotEmpty(memberId)) {
            point = this.baseMapper.getMemberPointsHistoryVO(1, memberId);
            variablePoint = this.baseMapper.getMemberPointsHistoryVO(0, memberId);

        } else {
            point = this.baseMapper.getALLMemberPointsHistoryVO(0);
            variablePoint = this.baseMapper.getALLMemberPointsHistoryVO(1);
        }
        memberPointsHistoryVO.setPoint(point == null ? 0 : point);
        memberPointsHistoryVO.setVariablePoint(variablePoint == null ? 0 : variablePoint);
        memberPointsHistoryVO.setVariablePoint(memberPointsHistoryVO.getPoint() - memberPointsHistoryVO.getVariablePoint());
        return memberPointsHistoryVO;
    }

    @Override
    public IPage<MemberPointsHistory> MemberPointsHistoryList(PageVO page, String memberId, String memberName) {
        LambdaQueryWrapper<MemberPointsHistory> lambdaQueryWrapper = new LambdaQueryWrapper<MemberPointsHistory>()
                .eq(memberId != null, MemberPointsHistory::getMemberId, memberId)
                .like(memberName != null, MemberPointsHistory::getMemberName, memberName);
        //如果排序为空，则默认创建时间倒序
        if (StringUtils.isEmpty(page.getSort())) {
            page.setSort("createTime");
            page.setOrder("desc");
        }
        return this.page(PageUtil.initPage(page), lambdaQueryWrapper);
    }
}