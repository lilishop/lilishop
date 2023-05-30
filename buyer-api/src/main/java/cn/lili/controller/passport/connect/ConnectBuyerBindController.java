package cn.lili.controller.passport.connect;


import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.connect.service.ConnectService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 买家端,app/小程序 联合登录
 *
 * @author Chopper
 * @since 2020-11-25 19:29
 */
@RestController
@Api(tags = "买家端,app/小程序 联合登录")
@RequestMapping("/buyer/passport/connect/bind")
public class ConnectBuyerBindController {

    @Autowired
    private ConnectService connectService;

    @ApiOperation(value = "unionID 绑定")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "unionID", value = "unionID", required = true, paramType = "query"),
            @ApiImplicitParam(name = "type", value = "type", required = true, paramType = "query")
    })
    @PostMapping
    public void unionIDBind(@RequestParam String unionID, @RequestParam String type) {
        connectService.bind(unionID, type);
    }

    @ApiOperation(value = "unionID 解绑")
    @ApiImplicitParam(name = "type", value = "type", required = true, paramType = "query")
    @PostMapping("/unbind")
    public void unionIDBind(@RequestParam String type) {
        connectService.unbind(type);
    }


    @GetMapping("/list")
    @ApiOperation(value = "绑定列表")
    public ResultMessage<List<String>> bindList() {
        return ResultUtil.data(connectService.bindList());
    }


}
