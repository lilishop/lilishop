package cn.lili.controller.member;

import cn.lili.common.security.context.UserContext;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.MemberPointsHistory;
import cn.lili.modules.member.entity.vo.MemberPointsHistoryVO;
import cn.lili.modules.member.service.MemberPointsHistoryService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 买家端,会员积分历史接口
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
@RestController
@Api(tags = "买家端,会员积分历史接口")
@RequestMapping("/buyer/member/memberPointsHistory")
public class PointsHistoryBuyerController {
    @Autowired
    private MemberPointsHistoryService memberPointsHistoryService;

    @ApiOperation(value = "分页获取")
    @GetMapping(value = "/getByPage")
    public ResultMessage<IPage<MemberPointsHistory>> getByPage(PageVO page) {

        LambdaQueryWrapper<MemberPointsHistory> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(MemberPointsHistory::getMemberId, UserContext.getCurrentUser().getId());
        queryWrapper.orderByDesc(MemberPointsHistory::getCreateTime);
        return ResultUtil.data(memberPointsHistoryService.page(PageUtil.initPage(page), queryWrapper));
    }

    @ApiOperation(value = "获取会员积分VO")
    @GetMapping(value = "/getMemberPointsHistoryVO")
    public ResultMessage<MemberPointsHistoryVO> getMemberPointsHistoryVO() {
        return ResultUtil.data(memberPointsHistoryService.getMemberPointsHistoryVO(UserContext.getCurrentUser().getId()));
    }


}
