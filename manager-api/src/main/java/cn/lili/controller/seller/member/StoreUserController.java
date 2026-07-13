package cn.lili.controller.seller.member;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.service.StoreService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * 店铺端,管理员接口
 *
 * @author Chopper
 * @since 2020/11/16 10:57
 */
@RestController
@Tag(name = "店铺端,管理员接口")
@RequestMapping("/store/member/user")
public class StoreUserController {
    @Autowired
    private MemberService memberService;

    @Autowired
    private StoreService storeService;


    @GetMapping("/info")
    @Operation(summary = "获取当前登录用户接口")
    public ResultMessage<Member> getUserInfo() {
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser != null) {
            Member member = memberService.findByUsername(tokenUser.getUsername());
            member.setPassword(null);
            return ResultUtil.data(member);
        }
        throw new ServiceException(ResultCode.USER_NOT_LOGIN);
    }

    @GetMapping("")
    @Operation(summary = "获取当前登录店铺接口")
    public ResultMessage<Store> getStoreInfo() {
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser != null) {
            Store store = storeService.getById(tokenUser.getStoreId());
            return ResultUtil.data(store);
        }
        throw new ServiceException(ResultCode.USER_NOT_LOGIN);
    }


}
