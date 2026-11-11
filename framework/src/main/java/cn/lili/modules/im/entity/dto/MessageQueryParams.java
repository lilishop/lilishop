package cn.lili.modules.im.entity.dto;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.im.entity.dos.ImMessage;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 历史消息查询参数
 *
 * @author Chopper
 * @version v1.0
 * 2022-01-20 17:16
 */
@EqualsAndHashCode(callSuper = true)
@Data
@Schema(description = "历史消息查询参数")
@NoArgsConstructor
@AllArgsConstructor
public class MessageQueryParams extends PageVO {

    private static final long serialVersionUID = 3504156704697214077L;

    @Schema(description = "聊天会话ID")
    private String talkId;

    @Schema(description = "最后一条消息ID")
    private String lastMessageId;

    @Schema(description = "获取消息数量")
    private Integer num;

    public LambdaQueryWrapper<ImMessage> initQueryWrapper() {
        if (CharSequenceUtil.isEmpty(talkId)) {
            throw new ServiceException(ResultCode.ERROR);
        }
        if (num == null || num > 50) {
            num = 50;
        }

        LambdaQueryWrapper<ImMessage> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.eq(ImMessage::getTalkId, talkId);
        if (CharSequenceUtil.isNotEmpty(lastMessageId)) {
            lambdaQueryWrapper.lt(ImMessage::getId, lastMessageId);
        }
        lambdaQueryWrapper.orderByDesc(ImMessage::getCreateTime);
        return lambdaQueryWrapper;
    }
}
