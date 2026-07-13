package cn.lili.controller.seller.permission;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.vo.StoreMenuVO;
import cn.lili.modules.member.service.StoreMenuService;
import cn.lili.modules.permission.entity.dos.Menu;
import cn.lili.modules.permission.entity.dto.MenuSearchParams;
import cn.lili.modules.permission.entity.vo.MenuVO;
import cn.lili.modules.permission.service.MenuService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;  
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 管理端,菜单管理接口
 *
 * @author Chopper
 * @since 2020/11/20 12:07
 */
@Slf4j
@RestController
@Tag(name = "店铺端,菜单管理接口")
@RequestMapping("/store/menu")
public class StoreMenuController {

    @Autowired
    private StoreMenuService storeMenuService;


    @Operation(description = "获取所有菜单")
    @GetMapping("/tree")
    public ResultMessage<List<StoreMenuVO>> getAllMenuList() {
        return ResultUtil.data(storeMenuService.tree());
    }

    @Operation(description = "获取所有菜单---根据当前用户角色")
    @GetMapping("/memberMenu")
    public ResultMessage<List<StoreMenuVO>> memberMenu() {
        return ResultUtil.data(storeMenuService.findUserTree());
    }
}
