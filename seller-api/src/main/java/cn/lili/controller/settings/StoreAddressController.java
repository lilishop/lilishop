package cn.lili.controller.settings;


import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.store.entity.dos.StoreAddress;
import cn.lili.modules.store.service.StoreAddressService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.Objects;

/**
 * 店铺端,商家地址（自提点）接口
 *
 * @author Bulbasaur
 * @since 2020/11/22 14:23
 */
@RestController
@Api(tags = "店铺端,商家地址（自提点）接口")
@RequestMapping("/store/member/storeAddress")
public class StoreAddressController {

    /**
     * 店铺自提点
     */
    @Autowired
    private StoreAddressService storeAddressService;

    @ApiOperation(value = "获取商家自提点分页")
    @GetMapping
    public ResultMessage<IPage<StoreAddress>> get(PageVO pageVo) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(storeAddressService.getStoreAddress(storeId, pageVo));
    }

    @ApiOperation(value = "获取商家自提点信息")
    @ApiImplicitParam(name = "id", value = "自提点ID", required = true, paramType = "path")
    @GetMapping("/{id}")
    public ResultMessage<StoreAddress> get(@PathVariable String id) {
        StoreAddress address = OperationalJudgment.judgment(storeAddressService.getById(id));
        return ResultUtil.data(address);
    }

    @ApiOperation(value = "添加")
    @PostMapping
    public ResultMessage<StoreAddress> add(@Valid StoreAddress storeAddress) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        storeAddress.setStoreId(storeId);
        storeAddressService.save(storeAddress);
        return ResultUtil.data(storeAddress);
    }

    @ApiOperation(value = "编辑")
    @ApiImplicitParam(name = "id", value = "自提点ID", required = true, paramType = "path")
    @PutMapping("/{id}")
    public ResultMessage<StoreAddress> edit(@PathVariable String id, @Valid StoreAddress storeAddress) {
        OperationalJudgment.judgment(storeAddressService.getById(id));
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        storeAddress.setId(id);
        storeAddress.setStoreId(storeId);
        storeAddressService.updateById(storeAddress);
        return ResultUtil.data(storeAddress);
    }

    @ApiOperation(value = "删除")
    @ApiImplicitParam(name = "id", value = "自提点ID", required = true, paramType = "path")
    @DeleteMapping(value = "/{id}")
    public ResultMessage<Object> delByIds(@PathVariable String id) {
        OperationalJudgment.judgment(storeAddressService.getById(id));
        storeAddressService.removeStoreAddress(id);
        return ResultUtil.success();
    }

}
