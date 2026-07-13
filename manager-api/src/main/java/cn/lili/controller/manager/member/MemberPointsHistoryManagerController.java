package cn.lili.controller.manager.member;
 
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.MemberPointsHistory;
import cn.lili.modules.member.entity.vo.MemberPointsHistoryVO;
import cn.lili.modules.member.entity.vo.MemberPointsStatisticsVO;
import cn.lili.modules.member.service.MemberPointsHistoryService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端,客户积分历史接口
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
@RestController
@Tag(name = "管理端,客户积分历史接口")
@RequestMapping("/manager/member/memberPointsHistory")
public class MemberPointsHistoryManagerController {
    @Autowired
    private MemberPointsHistoryService memberPointsHistoryService;

    @Operation(description = "分页获取")
    @Parameter(name = "page", description = "分页参数")
    @Parameter(name = "memberId", description = "客户ID")
    @Parameter(name = "memberName", description = "客户名称")
    @GetMapping("/getByPage")
    public ResultMessage<IPage<MemberPointsHistory>> getByPage(PageVO page, String memberId, String memberName) {
        return ResultUtil.data(memberPointsHistoryService.MemberPointsHistoryList(page, memberId, memberName));
    }

    @Operation(description = "获取客户积分VO")
    @Parameter(name = "memberId", description = "客户ID", required = true)
    @GetMapping("/getMemberPointsHistoryVO")
    public ResultMessage<MemberPointsHistoryVO> getMemberPointsHistoryVO(String memberId) {
        return ResultUtil.data(memberPointsHistoryService.getMemberPointsHistoryVO(memberId));
    }

    @Operation(description = "获取积分统计")
    @GetMapping("/queryMemberPointsStatistics")
    public ResultMessage<MemberPointsStatisticsVO> queryMemberPointsStatistics() {
        return ResultUtil.data(memberPointsHistoryService.queryMemberPointsStatistics());
    }




}
