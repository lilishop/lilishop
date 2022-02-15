package cn.lili.controller.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dos.Logistics;
import cn.lili.modules.system.service.LogisticsService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 买家端,物流公司接口
 *
 * @author Bulbasaur
 * @since 2020-05-5 15:10:16
 */
@RestController
@Api(tags = "买家端,物流公司接口")
@RequestMapping("/buyer/other/logistics")
public class LogisticsBuyerController {

    @Autowired
    private LogisticsService logisticsService;


    @ApiOperation(value = "分页获取物流公司")
    @GetMapping
    public ResultMessage<List<Logistics>> getByPage() {
        return ResultUtil.data(logisticsService.getOpenLogistics());
    }

}
