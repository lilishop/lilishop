package cn.lili.controller.manager.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.message.entity.dos.Message;
import cn.lili.modules.message.entity.vos.MessageVO;
import cn.lili.modules.message.service.MessageService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 管理端,消息发送管理接口
 *
 * @author pikachu
 * @since 2020-05-06 15:18:56
 */
@RestController
@Tag(name = "管理端,消息发送管理接口")
@RequestMapping("/manager/other/message")
public class MessageManagerController {
    @Autowired
    private MessageService messageService;


    @GetMapping
    @Operation(summary = "多条件分页获取")
    public ResultMessage<IPage<Message>> getByCondition(MessageVO messageVO,
                                                        PageVO pageVo) {
        return ResultUtil.data(messageService.getPage(messageVO, pageVo));
    }

    @PostMapping
    @Operation(summary = "发送消息")
    public ResultMessage<Boolean> sendMessage(Message message) {

        return ResultUtil.data(messageService.sendMessage(message));
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "删除消息")
    public ResultMessage<Boolean> deleteMessage(
            @Parameter(description = "消息id", required = true) @PathVariable String id) {

        return ResultUtil.data(messageService.deleteMessage(id));
    }

}
