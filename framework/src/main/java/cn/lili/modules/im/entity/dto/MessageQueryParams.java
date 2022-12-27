package cn.lili.modules.im.entity.dto;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.im.entity.dos.ImMessage;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.Data;

/**
 * MessageQueryParams
 *
 * @author Chopper
 * @version v1.0
 * 2022-01-20 17:16
 */
@Data
public class MessageQueryParams extends PageVO {
    /**
     * 聊天窗口
     */
    private String talkId;
    /**
     * 最后一个消息
     */
    private String lastMessageId;
    /**
     * 获取消息数量
     */
    private Integer num;

    public LambdaQueryWrapper<ImMessage> initQueryWrapper() {
        if (StringUtils.isEmpty(talkId)) {
            throw new ServiceException(ResultCode.ERROR);
        }
        if (num == null || num > 50) {
            num = 50;
        }

        LambdaQueryWrapper<ImMessage> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.eq(ImMessage::getTalkId, talkId);
        if (StringUtils.isNotEmpty(lastMessageId)) {
            lambdaQueryWrapper.lt(ImMessage::getId, lastMessageId);
        }
        lambdaQueryWrapper.orderByDesc(ImMessage::getCreateTime);
//        lambdaQueryWrapper.last("limit " + num);
        return lambdaQueryWrapper;
    }
}
