package cn.lili.controller.member;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.service.PaidMemberService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import java.util.*;
/**
 * 买家付费会员中心、购买、续费和价格预览。
 * @author Bulbasaur
 * @since 2026-11-04
 */
@RestController
@RequestMapping("/buyer/member/paid")
public class PaidMemberBuyerController {
    @Autowired private PaidMemberService service;
    private String memberId() { return Objects.requireNonNull(UserContext.getCurrentUser()).getId(); }
    @GetMapping("/current") public ResultMessage<?> current() { return ResultUtil.data(service.current(memberId())); }
    @PostMapping("/orders") public ResultMessage<?> buy(@RequestBody Map<String, Object> body) {
        return ResultUtil.data(service.create(memberId(), (String) body.get("requestId"), (String) body.get("configVersion"), Boolean.TRUE.equals(body.get("agreed"))));
    }
    @GetMapping("/orders") public ResultMessage<?> orders(PageVO page) { return ResultUtil.data(service.page(memberId(), null, null, page)); }
    @GetMapping("/orders/{sn}") public ResultMessage<?> order(@PathVariable String sn) { return ResultUtil.data(service.ownedOrder(memberId(), sn)); }
    @PostMapping("/orders/{sn}/cancel") public ResultMessage<?> cancel(@PathVariable String sn) { service.cancel(memberId(), sn); return ResultUtil.success(); }
    @GetMapping("/quotes") public ResultMessage<?> quotes(@RequestParam List<String> skuIds) { return ResultUtil.data(service.quotes(memberId(), skuIds)); }
}
