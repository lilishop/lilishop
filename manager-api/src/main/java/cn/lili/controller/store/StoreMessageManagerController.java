package cn.lili.controller.store;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.message.entity.dos.StoreMessage;
import cn.lili.modules.message.entity.vos.StoreMessageQueryVO;
import cn.lili.modules.message.service.StoreMessageService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * 管理端,店铺消息消息管理接口
 *
 * @author pikachu
 * @since 2020/12/6 16:09
 */
@RestController
@Api(tags = "管理端,店铺消息消息管理接口")
@RequestMapping("/manager/message/store")
public class StoreMessageManagerController {

    @Autowired
    private StoreMessageService storeMessageService;

    @GetMapping
    @ApiOperation(value = "多条件分页获取")
    public ResultMessage<IPage<StoreMessage>> getByCondition(StoreMessageQueryVO storeMessageQueryVO,
                                                             PageVO pageVo) {
        IPage<StoreMessage> page = storeMessageService.getPage(storeMessageQueryVO, pageVo);
        return ResultUtil.data(page);
    }

}
