package cn.lili.modules.sms.serviceimpl;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.sms.AliSmsUtil;
import cn.lili.modules.sms.entity.dos.SmsTemplate;
import cn.lili.modules.sms.mapper.SmsTemplateMapper;
import cn.lili.modules.sms.service.SmsTemplateService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;

/**
 * 短信模板业务层实现
 * @author Chopper
 * @since 2021/1/30 4:27 下午
 */
@Slf4j
@Service
public class SmsTemplateServiceImpl extends ServiceImpl<SmsTemplateMapper, SmsTemplate> implements SmsTemplateService {
    @Autowired
    private AliSmsUtil aliSmsUtil;


    @Override
    public void addSmsTemplate(SmsTemplate smsTemplate) {
        try {
            smsTemplate.setTemplateCode(aliSmsUtil.addSmsTemplate(smsTemplate));
            smsTemplate.setTemplateStatus(0);
            smsTemplate.setTemplateType(1);
            this.save(smsTemplate);
        } catch (Exception e) {
            log.error("添加短信模板错误",e);
        }
    }

    @Override
    public void deleteSmsTemplate(String id) {
        try {
            SmsTemplate smsTemplate = this.getById(id);
            if (smsTemplate.getTemplateCode() != null) {
                aliSmsUtil.deleteSmsTemplate(smsTemplate.getTemplateCode());
            }
            this.removeById(id);
        } catch (Exception e) {
            log.error("删除短信模板错误",e);
        }

    }

    @Override
    public void querySmsTemplate() {
        try {
            Map<String, Object> map;
            //获取未审核通过的签名列表
            List<SmsTemplate> list = list(new LambdaQueryWrapper<SmsTemplate>().eq(SmsTemplate::getTemplateStatus, 0));
            //查询签名状态
            for (SmsTemplate smsTemplate : list) {
                map = aliSmsUtil.querySmsTemplate(smsTemplate.getTemplateCode());
                smsTemplate.setTemplateStatus((Integer) map.get("TemplateStatus"));
                smsTemplate.setReason(map.get("Reason").toString());
                smsTemplate.setTemplateCode(map.get("TemplateCode").toString());
                this.updateById(smsTemplate);
            }
        } catch (Exception e) {
            log.error("查询短信模板错误",e);
        }
    }

    @Override
    public void modifySmsTemplate(SmsTemplate smsTemplate) {
        try {
            aliSmsUtil.modifySmsTemplate(smsTemplate);
            smsTemplate.setTemplateStatus(0);
            this.updateById(smsTemplate);
        } catch (Exception e) {
            log.error("重新提交短信模板错误",e);
        }
    }

    @Override
    public IPage<SmsTemplate> page(PageVO pageVO, Integer templateStatus) {
        return this.page(PageUtil.initPage(pageVO), new QueryWrapper<SmsTemplate>()
                .eq(templateStatus != null, "template_status", templateStatus));
    }
}
