package cn.lili.modules.sms.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.sms.entity.dos.SmsTemplate;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 签名申请业务层
 *
 * @author Bulbasaur
 * @since 2021/1/30 3:19 下午
 */
public interface SmsTemplateService extends IService<SmsTemplate> {

    /**
     * 添加短信模板
     *
     * @param smsTemplate 短信模板
     */
    void addSmsTemplate(SmsTemplate smsTemplate);

    /**
     * 删除短信模板
     *
     * @param templateCode 短信模板CODE
     */
    void deleteSmsTemplate(String templateCode);

    /**
     * 查询短信模板的审核状态
     */
    void querySmsTemplate();

    /**
     * 修改未通过审核的短信模板，并重新提交审核。
     *
     * @param smsTemplate 短信模板
     */
    void modifySmsTemplate(SmsTemplate smsTemplate);

    /**
     * 分页查询短信模板
     *
     * @param pageVO         分页参数
     * @param templateStatus 状态
     * @return
     */
    IPage<SmsTemplate> page(PageVO pageVO, Integer templateStatus);

}
