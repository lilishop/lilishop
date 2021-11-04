package cn.lili.modules.verification.entity.dto;

import cn.lili.modules.verification.entity.dos.VerificationSource;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 验证码资源缓存DTO
 *
 * @author Chopper
 * @since 2020/12/2 17:50
 */
@Data
public class VerificationDTO implements Serializable {


    /**
     * 缓存资源
     */
    List<VerificationSource> verificationResources;

    /**
     * 缓存滑块资源
     */
    List<VerificationSource> verificationSlider;

}