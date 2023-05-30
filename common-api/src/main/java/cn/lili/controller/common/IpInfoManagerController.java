package cn.lili.controller.common;

import cn.lili.common.utils.IpHelper;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;

/**
 * 管理端,IP接口
 *
 * @author Chopper
 * @since 2020-02-25 14:10:16
 */
@RestController
@Api(tags = "获取IP信息以及天气")
@RequestMapping("/common/common/ip")
public class IpInfoManagerController {
    @Autowired
    private IpHelper ipHelper;

    @RequestMapping(value = "/info", method = RequestMethod.GET)
    @ApiOperation(value = "IP及天气相关信息")
    public ResultMessage<Object> upload(HttpServletRequest request) {

        String result = ipHelper.getIpCity(request);
        return ResultUtil.data(result);
    }
}