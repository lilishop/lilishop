package cn.lili.controller.member;

import cn.lili.cache.limit.annotation.LimitPoint;
import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.MemberSign;
import cn.lili.modules.member.service.MemberSignService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 会员签到控制器
 *
 * @author pikachu
 * @since 2020/11/16 10:07 下午
 */
@RestController
@Api(tags = "买家端，会员签到API")
@RequestMapping("/buyer/members/sign")
public class MemberSignBuyerController {
    @Autowired
    private MemberSignService memberSignService;


    @PreventDuplicateSubmissions
    @PostMapping
    @ApiOperation(value = "会员签到")
    public ResultMessage<Boolean> memberSign() {
        return ResultUtil.data(memberSignService.memberSign());
    }

    @GetMapping
    @ApiOperation(value = "根据时间查询会员签到表，类型是YYYYmm")
    public ResultMessage<List<MemberSign>> memberSign(String time) {
        return ResultUtil.data(memberSignService.getMonthSignDay(time));
    }

}
