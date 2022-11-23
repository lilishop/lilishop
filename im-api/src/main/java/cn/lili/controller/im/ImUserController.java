package cn.lili.controller.im;


import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.service.StoreService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * @author Chopper
 */
@RestController
@Api(tags = "Im消息接口")
@RequestMapping("/lili/imUser")
@Transactional(rollbackFor = Exception.class)
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class ImUserController {

    private final MemberService memberService;

    @Autowired
    private StoreService storeService;

    @GetMapping
    @ApiOperation(value = "获取用户信息")
    public ResultMessage<Member> getImUser() {
        AuthUser authUser = UserContext.getCurrentUser();
        return ResultUtil.data(memberService.getById(authUser.getId()));
    }

    @GetMapping("/store")
    @ApiOperation(value = "获取用户信息")
    public ResultMessage<Store> getStoreUser() {
        AuthUser authUser = UserContext.getCurrentUser();
        return ResultUtil.data(storeService.getById(authUser.getStoreId()));
    }

}
