package cn.lili.controller.promotion;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dto.GiftCardCashBindDTO;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashMemberCardSearchParams;
import cn.lili.modules.promotion.entity.vos.GiftCardCashMemberCardVO;
import cn.lili.modules.promotion.service.GiftCardCashService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 买家端会员礼品卡接口（我的礼品卡、绑定、激活等）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@RestController
@Tag(name = "买家端,客户礼品卡接口")
@RequestMapping("/buyer/promotion/giftCardCash")
public class GiftCardCashBuyerController {

    @Autowired
    private GiftCardCashService giftCardCashService;

    @Operation(summary = "分页查询当前客户礼品卡")
    @GetMapping("/memberCard")
    public ResultMessage<IPage<GiftCardCashMemberCardVO>> pageMemberCards(
            GiftCardCashMemberCardSearchParams params, PageVO page) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        return ResultUtil.data(giftCardCashService.pageMemberCards(currentUser.getId(), params, page));
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "绑定礼品卡（输入卡号和兑换码）")
    @PostMapping("/memberCard/bind")
    public ResultMessage<Object> bindMemberCard(@Valid @RequestBody GiftCardCashBindDTO dto) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        giftCardCashService.bindMemberCard(currentUser.getId(), dto.getCardNo(), dto.getCardSecret());
        return ResultUtil.success();
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "激活礼品卡（激活后方可使用）")
    @PostMapping("/memberCard/{memberCardId}/activate")
    public ResultMessage<Object> activateMemberCard(@PathVariable String memberCardId) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        giftCardCashService.activateMemberCard(currentUser.getId(), memberCardId);
        return ResultUtil.success();
    }
}
