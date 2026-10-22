package cn.lili.controller.wallet;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.vo.MemberReceiptAddVO;
import cn.lili.modules.member.entity.vo.MemberReceiptVO;
import cn.lili.modules.member.service.MemberReceiptService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Objects;

/**
 * 买家端,客户发票接口
 *
 * @author paulG
 * @since 2021-03-29 14:10:16
 */
@RestController
@Tag(name = "买家端,客户发票接口")
@RequestMapping("/buyer/wallet/receipt")
public class MemberReceiptController {

    @Autowired
    private MemberReceiptService memberReceiptService;

    @Operation(summary = "查询客户发票列表")
    @GetMapping
    public ResultMessage<Object> page(MemberReceiptVO memberReceiptVO, PageVO page) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        // 安全防护：强制限制为当前登录用户 memberId，杜绝未传参数或伪造参数拉取其他用户发票抬头列表
        memberReceiptVO.setMemberId(currentUser.getId());
        return ResultUtil.data(memberReceiptService.getPage(memberReceiptVO, page));
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "新增客户发票")
    @PostMapping
    public ResultMessage<Object> add(MemberReceiptAddVO memberReceiptAddVO) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        // 安全防护：强制传入当前登录用户的 memberId
        return ResultUtil.data(memberReceiptService.addMemberReceipt(memberReceiptAddVO, currentUser.getId()));
    }

    @Operation(summary = "修改客户发票")
    @Parameter(name = "id", description = "客户发票id", required = true)
    @PutMapping("/{id}")
    public ResultMessage<Object> update(@PathVariable String id, MemberReceiptAddVO memberReceiptAddVO) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        // 安全防护：设置待修改记录主键，并传入当前用户 memberId 进行修改鉴权
        memberReceiptAddVO.setId(id);
        return ResultUtil.data(memberReceiptService.editMemberReceipt(memberReceiptAddVO, currentUser.getId()));
    }

    @Operation(summary = "客户发票删除")
    @Parameter(name = "id", description = "客户发票id", required = true)
    @DeleteMapping("/{id}")
    public ResultMessage<Boolean> deleteMessage(@PathVariable String id) {
        // 安全防护：deleteMemberReceipt 内部核验当前发票归属，防止越权删除他人发票记录
        return ResultUtil.data(memberReceiptService.deleteMemberReceipt(id));
    }

}
