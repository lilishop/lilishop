package cn.lili.modules.message.serviceimpl;

import cn.lili.common.sms.AliSmsUtil;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.message.entity.dos.SmsTemplate;
import cn.lili.modules.message.mapper.SmsTemplateMapper;
import cn.lili.modules.message.service.SmsTemplateService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 短信模板业务层实现
 * @author Chopper
 * @date 2021/1/30 4:27 下午
 */
@Service
@Transactional
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
            e.printStackTrace();
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
            e.printStackTrace();
        }

    }

    @Override
    public void querySmsTemplate() {
        try {
            Map<String, Object> map = new HashMap<>();
            //获取未审核通过的签名列表
            List<SmsTemplate> list = list(new LambdaQueryWrapper<SmsTemplate>().eq(SmsTemplate::getTemplateStatus, 0));
            //查询签名状态
            for (SmsTemplate smsTemplate : list) {
                map = aliSmsUtil.querySmsTemplate(smsTemplate.getTemplateName());
                smsTemplate.setTemplateStatus((Integer) map.get("TemplateStatus"));
                smsTemplate.setReason(map.get("Reason").toString());
                smsTemplate.setTemplateCode(map.get("TemplateCode").toString());
                this.updateById(smsTemplate);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public void modifySmsTemplate(SmsTemplate smsTemplate) {
        try {
            aliSmsUtil.modifySmsTemplate(smsTemplate);
            smsTemplate.setTemplateStatus(0);
            this.updateById(smsTemplate);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public IPage<SmsTemplate> page(PageVO pageVO, Integer templateStatus) {
        return this.page(PageUtil.initPage(pageVO), new QueryWrapper<SmsTemplate>()
                .eq(templateStatus != null, "template_status", templateStatus));
    }
}
