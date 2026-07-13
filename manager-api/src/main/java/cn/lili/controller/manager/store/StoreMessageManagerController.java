package cn.lili.controller.manager.store;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.message.entity.dos.StoreMessage;
import cn.lili.modules.message.entity.vos.StoreMessageQueryVO;
import cn.lili.modules.message.service.StoreMessageService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * 管理端,店铺消息管理接口
 *
 * @author pikachu
 * @since 2020/12/6 16:09
 */
@RestController
@Tag(name = "管理端,店铺消息管理接口")
@RequestMapping("/manager/other/storeMessage")
public class StoreMessageManagerController {

    @Autowired
    private StoreMessageService storeMessageService;

    @GetMapping
    @Operation(summary = "多条件分页获取")
    @Parameter(name = "storeMessageQueryVO", description = "店铺消息查询参数", required = true)
    @Parameter(name = "pageVo", description = "分页参数", required = true)
    public ResultMessage<IPage<StoreMessage>> getByCondition(StoreMessageQueryVO storeMessageQueryVO,
                                                             PageVO pageVo) {
        IPage<StoreMessage> page = storeMessageService.getPage(storeMessageQueryVO, pageVo);
        return ResultUtil.data(page);
    }

}
