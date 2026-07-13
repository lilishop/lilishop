package cn.lili.controller.manager.message;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.system.entity.dos.ServiceNotice;
import cn.lili.modules.system.service.ServiceNoticeService;
import io.swagger.v3.oas.annotations.Parameter;

import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "管理端,服务订阅消息接口")
@RequestMapping("/manager/message/serviceNotice")
public class ServiceNoticeManagerController {
    @Autowired
    private ServiceNoticeService serviceNoticeService;

    @Operation(summary = "查看服务订阅消息详情")
    @Parameter(name = "id", description = "服务订阅消息ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<ServiceNotice> get(@PathVariable String id) {
        ServiceNotice serviceNotice = serviceNoticeService.getById(id);
        return ResultUtil.data(serviceNotice);
    }

    @Operation(summary = "分页获取服务订阅消息")
    @Parameter(name = "entity", description = "服务订阅消息查询实体")
    @Parameter(name = "searchVo", description = "分页查询参数")
    @Parameter(name = "page", description = "分页参数")
    @GetMapping("/page")
    public ResultMessage<IPage<ServiceNotice>> getByPage(ServiceNotice entity,
                                                         SearchVO searchVo,
                                                         PageVO page) {
        return ResultUtil.data(serviceNoticeService.getByPage(entity, searchVo, page));
    }

    @Operation(summary = "新增服务订阅消息")
    @Parameter(name = "serviceNotice", description = "服务订阅消息实体", required = true)
    @PostMapping
    public ResultMessage<ServiceNotice> save(ServiceNotice serviceNotice) {
        //标记平台消息
        serviceNotice.setStoreId("-1");
        serviceNoticeService.saveOrUpdate(serviceNotice);
        return ResultUtil.data(serviceNotice);
    }

    @Operation(summary = "更新服务订阅消息")
    @Parameter(name = "id", description = "服务订阅消息ID", required = true)
    @Parameter(name = "serviceNotice", description = "服务订阅消息实体", required = true)
    @PostMapping("/{id}")
    public ResultMessage<ServiceNotice> update(@PathVariable String id, ServiceNotice serviceNotice) {
        serviceNoticeService.saveOrUpdate(serviceNotice);
        return ResultUtil.data(serviceNotice);
    }

    @Operation(summary = "删除服务订阅消息")
    @Parameter(name = "ids", description = "服务订阅消息ID列表", required = true)
    @DeleteMapping("/{ids}")
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        serviceNoticeService.removeByIds(ids);
        return ResultUtil.success();
    }
}
