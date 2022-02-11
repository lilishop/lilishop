package cn.lili.modules.verification;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 验证码sdk
 *
 * @author Chopper
 * @version v4.0
 * @since 2020/11/17 15:43
 */
@Component
public class VerificationSDK {

    @Autowired
    private Cache cache;

    /**
     * 生成一个token 用于获取校验中心的验证码逻辑
     *
     * @return
     */
    public boolean checked(String verificationKey, String uuid) {
        //生成校验KEY，在验证码服务做记录
        String key = CachePrefix.VERIFICATION_KEY.getPrefix() + verificationKey;
        cache.get(key);
        return true;
    }


}
