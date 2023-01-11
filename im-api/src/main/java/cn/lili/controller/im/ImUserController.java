package cn.lili.controller.im;


import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.FootPrint;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.FootPrintQueryParams;
import cn.lili.modules.member.service.FootprintService;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.service.StoreService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * @author Chopper
 */
@RestController
@Api(tags = "Im消息接口")
@RequestMapping("/im/user")
@Transactional(rollbackFor = Exception.class)
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class ImUserController {

    private final MemberService memberService;

    @Autowired
    private StoreService storeService;

    @Autowired
    private FootprintService footprintService;

    @GetMapping
    @ApiOperation(value = "获取用户信息")
    public ResultMessage<Member> getImUser() {
        AuthUser authUser = UserContext.getCurrentUser();
        return ResultUtil.data(memberService.getById(authUser.getId()));
    }

    @GetMapping("/store")
    @ApiOperation(value = "获取店铺信息")
    public ResultMessage<Store> getStoreUser() {
        AuthUser authUser = UserContext.getCurrentUser();
        return ResultUtil.data(storeService.getById(authUser.getStoreId()));
    }

    @GetMapping("/{memberId}")
    @ApiImplicitParam(name = "memberId", value = "店铺Id", required = true, dataType = "String", paramType = "path")
    @ApiOperation(value = "获取用户信息")
    public ResultMessage<Member> getImUserDetail(@PathVariable String memberId) {
        return ResultUtil.data(memberService.getById(memberId));
    }

    @GetMapping("/store/{storeId}")
    @ApiImplicitParam(name = "storeId", value = "店铺Id", required = true, dataType = "String", paramType = "path")
    @ApiOperation(value = "获取店铺信息")
    public ResultMessage<Store> getStoreUserDetail(@PathVariable String storeId) {
        return ResultUtil.data(storeService.getById(storeId));
    }

    @GetMapping("/history")
    @ApiOperation(value = "获取会员的历史足迹")
    public ResultMessage<IPage<EsGoodsIndex>> getMemberHistory(FootPrintQueryParams params) {
        return ResultUtil.data(footprintService.footPrintPage(params));
    }

}
