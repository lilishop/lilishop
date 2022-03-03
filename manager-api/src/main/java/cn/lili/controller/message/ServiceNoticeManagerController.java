package cn.lili.controller.message;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.system.entity.dos.ServiceNotice;
import cn.lili.modules.system.service.ServiceNoticeService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 管理端,服务订阅消息接口
 *
 * @author Chopper
 * @since 2020/11/17 4:33 下午
 */
@RestController
@Api(tags = "管理端,服务订阅消息接口")
@RequestMapping("/manager/message/serviceNotice")
public class ServiceNoticeManagerController {
    @Autowired
    private ServiceNoticeService serviceNoticeService;

    @ApiOperation(value = "查看服务订阅消息详情")
    @GetMapping(value = "/{id}")
    public ResultMessage<ServiceNotice> get(@PathVariable String id) {
        ServiceNotice serviceNotice = serviceNoticeService.getById(id);
        return ResultUtil.data(serviceNotice);
    }

    @ApiOperation(value = "分页获取服务订阅消息")
    @GetMapping(value = "/page")
    public ResultMessage<IPage<ServiceNotice>> getByPage(ServiceNotice entity,
                                                         SearchVO searchVo,
                                                         PageVO page) {
        IPage<ServiceNotice> data = serviceNoticeService.page(PageUtil.initPage(page), PageUtil.initWrapper(entity, searchVo));
        return ResultUtil.data(data);
    }

    @ApiOperation(value = "新增服务订阅消息")
    @PostMapping
    public ResultMessage<ServiceNotice> save(ServiceNotice serviceNotice) {
        //标记平台消息
        serviceNotice.setStoreId("-1");
        serviceNoticeService.saveOrUpdate(serviceNotice);
        return ResultUtil.data(serviceNotice);
    }

    @ApiOperation(value = "更新服务订阅消息")
    @PostMapping("/{id}")
    public ResultMessage<ServiceNotice> update(@PathVariable String id, ServiceNotice serviceNotice) {
        serviceNoticeService.saveOrUpdate(serviceNotice);
        return ResultUtil.data(serviceNotice);
    }

    @ApiOperation(value = "删除服务订阅消息")
    @DeleteMapping(value = "/{ids}")
    public ResultMessage<Object> delAllByIds(@PathVariable List ids) {
        serviceNoticeService.removeByIds(ids);
        return ResultUtil.success();
    }
}
