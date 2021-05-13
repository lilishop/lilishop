package cn.lili.modules.member.serviceimpl;


import cn.lili.common.utils.StringUtils;
import cn.lili.modules.member.entity.dos.MemberPointsHistory;
import cn.lili.modules.member.entity.vo.MemberPointsHistoryVO;
import cn.lili.modules.member.mapper.MemberPointsHistoryMapper;
import cn.lili.modules.member.service.MemberPointsHistoryService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 会员积分历史业务层实现
 *
 * @author Bulbasaur
 * @date 2020-02-25 14:10:16
 */
@Service
@Transactional
public class MemberPointsHistoryServiceImpl extends ServiceImpl<MemberPointsHistoryMapper, MemberPointsHistory> implements MemberPointsHistoryService {

    @Autowired
    private MemberPointsHistoryMapper memberPointsHistoryMapper;

    @Override
    public MemberPointsHistoryVO getMemberPointsHistoryVO(String memberId) {
        MemberPointsHistoryVO memberPointsHistoryVO = new MemberPointsHistoryVO();
        Long point = 0L;
        Long variablePoint = 0L;

        if (StringUtils.isNotEmpty(memberId)) {
            point = memberPointsHistoryMapper.getMemberPointsHistoryVO(1, memberId);
            variablePoint = memberPointsHistoryMapper.getMemberPointsHistoryVO(0, memberId);

        } else {
            point = memberPointsHistoryMapper.getALLMemberPointsHistoryVO(0);
            variablePoint = memberPointsHistoryMapper.getALLMemberPointsHistoryVO(1);
        }
        memberPointsHistoryVO.setPoint(point == null ? 0 : point);
        memberPointsHistoryVO.setVariablePoint(variablePoint == null ? 0 : variablePoint);
        memberPointsHistoryVO.setVariablePoint(memberPointsHistoryVO.getPoint() - memberPointsHistoryVO.getVariablePoint());
        return memberPointsHistoryVO;
    }
}