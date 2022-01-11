package cn.lili.modules.sms.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.sms.entity.dos.SmsSign;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 签名申请业务层
 *
 * @author Bulbasaur
 * @since 2021/1/30 3:19 下午
 */
public interface SmsSignService extends IService<SmsSign> {

    /**
     * 添加短信签名
     *
     * @param smsSign 短信签名
     */
    void addSmsSign(SmsSign smsSign);

    /**
     * 删除短信签名
     *
     * @param id 短信签名id
     */
    void deleteSmsSign(String id);

    /**
     * 查询短信签名申请状态
     */
    void querySmsSign();

    /**
     * 修改未审核通过的短信签名，并重新提交审核。
     *
     * @param smsSign 短信签名
     */
    void modifySmsSign(SmsSign smsSign);


    /**
     * 分页查询短信签名
     *
     * @param pageVO  分页参数
     * @param signStatus 短信签名状态
     * @return 分页数据
     */
    IPage<SmsSign> page(PageVO pageVO, Integer signStatus);

}
