package cn.lili.controller.message;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.message.entity.dos.MemberMessage;
import cn.lili.modules.message.entity.vos.MemberMessageQueryVO;
import cn.lili.modules.message.service.MemberMessageService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * 管理端,会员消息消息管理接口
 *
 * @author pikachu
 * @since 2020/12/6 16:09
 */
@RestController
@Api(tags = "管理端,会员消息消息管理接口")
@RequestMapping("/manager/message/member")
public class MemberMessageManagerController {
    @Autowired
    private MemberMessageService memberMessageService;


    @GetMapping
    @ApiOperation(value = "多条件分页获取")
    public ResultMessage<IPage<MemberMessage>> getByCondition(MemberMessageQueryVO memberMessageQueryVO,
                                                              PageVO pageVo) {
        IPage<MemberMessage> page = memberMessageService.getPage(memberMessageQueryVO, pageVo);
        return ResultUtil.data(page);
    }

}
