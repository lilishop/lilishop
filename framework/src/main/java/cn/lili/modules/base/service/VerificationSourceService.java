package cn.lili.modules.base.service;

import cn.lili.common.cache.CachePrefix;
import cn.lili.modules.base.entity.dos.VerificationSource;
import cn.lili.modules.base.entity.vo.VerificationVO;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 验证码资源维护 业务层
 *
 * @author Chopper
 * @date 2020/11/17 3:44 下午
 */
public interface VerificationSourceService extends IService<VerificationSource> {

    /**
     * 缓存
     */
    String VERIFICATION_CACHE = CachePrefix.VERIFICATION.getPrefix();


    /**
     * 初始化缓存
     *
     * @return
     */
    VerificationVO initCache();

    /**
     * 获取验证缓存
     *
     * @return 验证码
     */
    VerificationVO getVerificationCache();
}