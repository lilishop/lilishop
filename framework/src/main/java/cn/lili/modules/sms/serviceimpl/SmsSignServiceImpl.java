package cn.lili.modules.sms.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.sms.AliSmsUtil;
import cn.lili.modules.sms.entity.dos.SmsSign;
import cn.lili.modules.sms.mapper.SmsSignMapper;
import cn.lili.modules.sms.service.SmsSignService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 短信签名业务层实现
 * @author Chopper
 * @since 2021/1/30 4:27 下午
 */
@Slf4j
@Service
public class SmsSignServiceImpl extends ServiceImpl<SmsSignMapper, SmsSign> implements SmsSignService {
    @Autowired
    private AliSmsUtil aliSmsUtil;

    @Override
    public void addSmsSign(SmsSign smsSign) {
        try {
            //如果短信签名已存在，不能重复申请
            if (this.getOne(new QueryWrapper<SmsSign>().eq("sign_name", smsSign.getSignName())) != null) {
                throw new ServiceException(ResultCode.SMS_SIGN_EXIST_ERROR);
            }
            aliSmsUtil.addSmsSign(smsSign);
            smsSign.setSignStatus(0);
            this.save(smsSign);
        } catch (Exception e) {
            log.error("添加短信签名错误",e);
        }
    }

    @Override
    public void deleteSmsSign(String id) {
        try {
            SmsSign smsSign = this.getById(id);
            if (smsSign != null) {
                aliSmsUtil.deleteSmsSign(smsSign.getSignName());
                this.removeById(id);
            }
        } catch (Exception e) {
            log.error("删除短信签名错误",e);
        }

    }

    @Override
    public void querySmsSign() {
        try {
            Map<String, Object> map = new HashMap<>(16);
            //获取未审核通过的签名列表
            List<SmsSign> list = list(new LambdaQueryWrapper<SmsSign>().ne(SmsSign::getSignStatus, 1));
            //查询签名状态
            for (SmsSign smsSign : list) {
                map = aliSmsUtil.querySmsSign(smsSign.getSignName());
                smsSign.setSignStatus((Integer) map.get("SignStatus"));
                smsSign.setReason(map.get("Reason").toString());
                this.updateById(smsSign);
            }
        } catch (Exception e) {
            log.error("查询短信签名错误",e);
        }
    }

    @Override
    public void modifySmsSign(SmsSign smsSign) {
        try {
            aliSmsUtil.modifySmsSign(smsSign);
            this.updateById(smsSign);
        } catch (Exception e) {
            log.error("更新短信签名错误",e);
        }
    }

    @Override
    public IPage<SmsSign> page(PageVO pageVO, Integer signStatus) {
        return this.page(PageUtil.initPage(pageVO), new QueryWrapper<SmsSign>()
                .eq(signStatus != null, "sign_status", signStatus));
    }
}
