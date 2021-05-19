package cn.lili.controller.member;

import cn.lili.common.utils.PageUtil;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dos.ServiceNotice;
import cn.lili.modules.system.service.ServiceNoticeService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * 买家端,会员站服务消息接口
 *
 * @author Chopper
 * @date: 2020/11/17 2:31 下午
 */
@RestController
@RequestMapping("/service/notice")
@Api(tags = "买家端,会员站服务消息接口")
public class ServiceNoticeBuyerController {

    /**
     * 服务消息
     */
    @Autowired
    private ServiceNoticeService serviceNoticeService;

    @ApiOperation(value = "获取消息详情")
    @ApiImplicitParam(name = "id", value = "商品ID", required = true, dataType = "Long", paramType = "path")
    @GetMapping(value = "/{id}")
    public ResultMessage<ServiceNotice> get(@PathVariable String id) {
        ServiceNotice serviceNotice = serviceNoticeService.getById(id);
        return ResultUtil.data(serviceNotice);
    }

    @ApiOperation(value = "分页获取服务消息")
    @GetMapping
    @ApiImplicitParam(name = "storeId", value = "商家id，默认为-1，代表平台消息，如果查询某商家发布的消息，传递商家id即可", dataType = "int", paramType = "query")
    public ResultMessage<IPage<ServiceNotice>> getByPage(PageVO page, String storeId) {
        ServiceNotice serviceNotice = new ServiceNotice();
        if (storeId == null) {
            storeId = "-1";
        }
        serviceNotice.setStoreId(storeId);
        IPage<ServiceNotice> data = serviceNoticeService.page(PageUtil.initPage(page));
        return ResultUtil.data(data);
    }
}
