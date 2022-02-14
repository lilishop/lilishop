package cn.lili.controller.message;


import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.message.entity.dos.StoreMessage;
import cn.lili.modules.message.entity.enums.MessageStatusEnum;
import cn.lili.modules.message.entity.vos.StoreMessageQueryVO;
import cn.lili.modules.message.service.StoreMessageService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * 店铺端,消息接口
 *
 * @author Bulbasaur
 * @since 2020/11/22 14:23
 */
@RestController
@Api(tags = "店铺端,消息接口")
@RequestMapping("/store/message/storeMessage")
public class StoreMessageController {

    /**
     * 商家消息
     */
    @Autowired
    private StoreMessageService storeMessageService;

    @ApiOperation(value = "获取商家消息")
    @ApiImplicitParam(name = "status", value = "状态", required = true, paramType = "query")
    @GetMapping
    public ResultMessage<IPage<StoreMessage>> getPage(String status, PageVO pageVo) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        StoreMessageQueryVO storeMessageQueryVO = new StoreMessageQueryVO();
        storeMessageQueryVO.setStatus(status);
        storeMessageQueryVO.setStoreId(storeId);
        IPage<StoreMessage> page = storeMessageService.getPage(storeMessageQueryVO, pageVo);
        return ResultUtil.data(page);
    }


    @ApiOperation(value = "获取商家消息总汇")
    @GetMapping("/all")
    public ResultMessage<Map<String, Object>> getPage(PageVO pageVo) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        //返回值定义
        Map<String, Object> map = new HashMap<>(4);
        StoreMessageQueryVO storeMessageQueryVO = new StoreMessageQueryVO();
        storeMessageQueryVO.setStoreId(storeId);
        //未读消息
        storeMessageQueryVO.setStatus(MessageStatusEnum.UN_READY.name());
        IPage<StoreMessage> page = storeMessageService.getPage(storeMessageQueryVO, pageVo);
        map.put("UN_READY", page);
        //已读消息
        storeMessageQueryVO.setStatus(MessageStatusEnum.ALREADY_READY.name());
        page = storeMessageService.getPage(storeMessageQueryVO, pageVo);
        map.put("ALREADY_READY", page);
        //回收站
        storeMessageQueryVO.setStatus(MessageStatusEnum.ALREADY_REMOVE.name());
        page = storeMessageService.getPage(storeMessageQueryVO, pageVo);
        map.put("ALREADY_REMOVE", page);
        return ResultUtil.data(map);
    }

    @ApiOperation(value = "已读操作")
    @ApiImplicitParam(name = "id", value = "店铺消息id", required = true, paramType = "path")
    @PutMapping("/{id}/read")
    public ResultMessage<Boolean> readMessage(@PathVariable String id) {
        OperationalJudgment.judgment(storeMessageService.getById(id));
        Boolean result = storeMessageService.editStatus(MessageStatusEnum.ALREADY_READY.name(), id);
        return ResultUtil.data(result);

    }

    @ApiOperation(value = "回收站还原消息")
    @ApiImplicitParam(name = "id", value = "店铺消息id", required = true, paramType = "path")
    @PutMapping("/{id}/reduction")
    public ResultMessage<Boolean> reductionMessage(@PathVariable String id) {
        OperationalJudgment.judgment(storeMessageService.getById(id));
        Boolean result = storeMessageService.editStatus(MessageStatusEnum.ALREADY_READY.name(), id);
        return ResultUtil.data(result);

    }

    @ApiOperation(value = "删除操作")
    @ApiImplicitParam(name = "id", value = "店铺消息id", required = true, paramType = "path")
    @DeleteMapping("/{id}/delete")
    public ResultMessage<Boolean> deleteMessage(@PathVariable String id) {
        OperationalJudgment.judgment(storeMessageService.getById(id));
        Boolean result = storeMessageService.editStatus(MessageStatusEnum.ALREADY_REMOVE.name(), id);
        return ResultUtil.data(result);

    }

    @ApiOperation(value = "彻底删除操作")
    @ApiImplicitParam(name = "id", value = "店铺消息id", required = true, paramType = "path")
    @DeleteMapping("/{id}")
    public ResultMessage<Boolean> disabled(@PathVariable String id) {
        OperationalJudgment.judgment(storeMessageService.getById(id));
        Boolean result = storeMessageService.deleteByMessageId(id);
        return ResultUtil.data(result);
    }


}
