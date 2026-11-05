package cn.lili.controller.member;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.vo.MemberCurrentBenefitVO;
import cn.lili.modules.member.service.MemberBenefitService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

/**
 * 买家端,客户权益接口
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@RestController
@Tag(name = "买家端,客户权益接口")
@RequestMapping("/buyer/member/benefit")
public class MemberBenefitBuyerController {

    @Autowired
    private MemberBenefitService memberBenefitService;

    @Operation(summary = "获取当前客户权益及等级升级经验进度")
    @GetMapping("/current")
    public ResultMessage<MemberCurrentBenefitVO> current() {
        String memberId = Objects.requireNonNull(UserContext.getCurrentUser()).getId();
        return ResultUtil.data(memberBenefitService.getCurrentMemberBenefits(memberId));
    }
}
