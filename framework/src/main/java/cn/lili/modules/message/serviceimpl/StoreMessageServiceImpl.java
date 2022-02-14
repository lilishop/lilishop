package cn.lili.modules.message.serviceimpl;


import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.message.entity.dos.StoreMessage;
import cn.lili.modules.message.entity.vos.StoreMessageQueryVO;
import cn.lili.modules.message.mapper.StoreMessageMapper;
import cn.lili.modules.message.service.StoreMessageService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.elasticsearch.ResourceNotFoundException;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 消息发送业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 3:48 下午
 */
@Service
public class StoreMessageServiceImpl extends ServiceImpl<StoreMessageMapper, StoreMessage> implements StoreMessageService {

    @Override
    public boolean deleteByMessageId(String messageId) {
        StoreMessage storeMessage = this.getById(messageId);
        if (storeMessage != null) {
            return this.removeById(messageId);
        }
        return false;

    }

    @Override
    public IPage<StoreMessage> getPage(StoreMessageQueryVO storeMessageQueryVO, PageVO pageVO) {

        QueryWrapper<StoreMessage> queryWrapper = new QueryWrapper<>();
        //消息id查询
        if (CharSequenceUtil.isNotEmpty(storeMessageQueryVO.getMessageId())) {
            queryWrapper.eq("message_id", storeMessageQueryVO.getMessageId());
        }
        //商家id
        if (CharSequenceUtil.isNotEmpty(storeMessageQueryVO.getStoreId())) {
            queryWrapper.eq("store_id", storeMessageQueryVO.getStoreId());
        }
        //状态查询
        if (storeMessageQueryVO.getStatus() != null) {
            queryWrapper.eq("status", storeMessageQueryVO.getStatus());
        }
        queryWrapper.orderByDesc("status");
        return this.baseMapper.queryByParams(PageUtil.initPage(pageVO), queryWrapper);

    }

    @Override
    public boolean save(List<StoreMessage> messages) {
        return saveBatch(messages);
    }

    @Override
    public boolean editStatus(String status, String id) {
        StoreMessage storeMessage = this.getById(id);
        if (storeMessage != null) {
            //校验权限
            if (!storeMessage.getStoreId().equals(UserContext.getCurrentUser().getStoreId())) {
                throw new ResourceNotFoundException(ResultCode.USER_AUTHORITY_ERROR.message());
            }
            storeMessage.setStatus(status);
            return this.updateById(storeMessage);
        }
        return false;
    }
}