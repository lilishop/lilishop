package cn.lili.modules.system.entity.vo;

import cn.lili.modules.system.entity.dos.VerificationSource;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 验证码资源缓存VO
 *
 * @author Chopper
 * @since 2020/12/2 17:50
 */
@Data
public class VerificationVO implements Serializable {


    /**
     * 缓存资源
     */
    List<VerificationSource> verificationResources;

    /**
     * 缓存滑块资源
     */
    List<VerificationSource> verificationSlider;

}