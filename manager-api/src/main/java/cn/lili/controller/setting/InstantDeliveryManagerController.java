package cn.lili.controller.setting;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dos.InstantDelivery;
import cn.lili.modules.system.entity.plugin.ConfigItem;
import cn.lili.modules.system.entity.vo.InstantDeliveryVO;
import cn.lili.modules.system.service.InstantDeliveryService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 管理端,即时配送接口
 *
 * @author pikachu
 * @date: 2020/11/17 7:56 下午
 */
@RestController
@Api(tags = "管理端,即时配送接口")
@RequestMapping("/manager/instant-delivery")
public class InstantDeliveryManagerController {
    @Autowired
    private InstantDeliveryService instantDeliveryService;

    @GetMapping(value = "/getByPage")
    @ApiOperation(value = "分页获取")
    public ResultMessage<IPage<InstantDeliveryVO>> getByPage(PageVO page) {
        //查询数据
        IPage<InstantDelivery> data = instantDeliveryService.page(PageUtil.initPage(page));
        //组织数据结构
        IPage<InstantDeliveryVO> newData = instantDeliveryService.getInstantDeliveryPage(data, page);
        System.out.println();
        //返回数据
        return ResultUtil.data(newData);
    }

    @ApiOperation(value = "修改即时配送方案参数", response = InstantDeliveryVO.class)
    @PutMapping(value = "/{bean}/config")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "bean", value = "即时配送bean", required = true, dataType = "String", paramType = "path"),
            @ApiImplicitParam(name = "config", value = "即时配送参数", required = true, dataType = "String", paramType = "body")
    })
    public ResultMessage<InstantDeliveryVO> edit(@PathVariable String bean, @RequestBody List<ConfigItem> config) {
        InstantDeliveryVO instantDeliveryVO = new InstantDeliveryVO();
        instantDeliveryVO.setDeliveryBean(bean);
        instantDeliveryVO.setConfigItems(config);
        return ResultUtil.data(this.instantDeliveryService.edit(instantDeliveryVO));
    }

    @ApiOperation(value = "获取即时配送的配置", response = InstantDeliveryVO.class)
    @GetMapping("/{bean}")
    @ApiImplicitParam(name = "bean", value = "即时配送bean id", required = true, dataType = "String", paramType = "path")
    public ResultMessage<InstantDeliveryVO> getInstantDeliverySetting(@PathVariable String bean) {
        return ResultUtil.data(this.instantDeliveryService.getInstantDeliveryConfig(bean));
    }


    @ApiOperation(value = "开启即时配送方案", response = String.class)
    @PutMapping("/{bean}/open")
    @ApiImplicitParam(name = "bean", value = "bean", required = true, dataType = "String", paramType = "path")
    public ResultMessage<String> open(@PathVariable String bean) {
        this.instantDeliveryService.openInstantDelivery(bean);
        return ResultUtil.success();
    }


    @ApiOperation(value = "修改封面图片", response = String.class)
    @PutMapping("/{bean}/image")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "bean", value = "即时配送bean", required = true, dataType = "String", paramType = "path"),
            @ApiImplicitParam(name = "images", value = "封面图片", required = true, dataType = "String", paramType = "query")
    })
    public ResultMessage<Boolean> open(@PathVariable String bean, String images) {
        InstantDelivery instantDelivery = this.instantDeliveryService.getOne(new QueryWrapper<InstantDelivery>().eq("delivery_bean", bean));
        if (instantDelivery != null) {
            instantDelivery.setImages(images);
            return ResultUtil.data(instantDeliveryService.updateById(instantDelivery));
        }
        return ResultUtil.data(false);
    }
}
