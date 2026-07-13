package cn.lili.controller.manager.message;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.message.entity.dos.MemberMessage;
import cn.lili.modules.message.entity.vos.MemberMessageQueryVO;
import cn.lili.modules.message.service.MemberMessageService;
import io.swagger.v3.oas.annotations.Parameter;

import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * 管理端,客户消息管理接口
 *
 * @author pikachu
 * @since 2020/12/6 16:09
 */
@RestController
@Tag(name = "管理端,客户消息管理接口")
@RequestMapping("/manager/other/memberMessage")
public class MemberMessageManagerController {
    @Autowired
    private MemberMessageService memberMessageService;


    @GetMapping
    @Operation(summary = "多条件分页获取")
    @Parameter(name = "memberMessageQueryVO", description = "客户消息查询VO")
    @Parameter(name = "pageVo", description = "分页参数")
    public ResultMessage<IPage<MemberMessage>> getByCondition(MemberMessageQueryVO memberMessageQueryVO,
                                                              PageVO pageVo) {
        IPage<MemberMessage> page = memberMessageService.getPage(memberMessageQueryVO, pageVo);
        return ResultUtil.data(page);
    }

}
