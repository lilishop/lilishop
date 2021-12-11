package cn.lili.modules.verification.service;

import cn.lili.modules.verification.entity.enums.VerificationEnums;

import java.io.IOException;
import java.util.Map;

/**
 * 验证码模块
 *
 * @author Bulbasaur
 * @since 2021/7/9 1:42 上午
 */
public interface VerificationService {
    /**
     * 获取校验对象
     *
     * @param verificationEnums 校验枚举
     * @param uuid              uuid
     * @return 校验对象
     * @throws IOException 校验错误
     */
    Map<String, Object> createVerification(VerificationEnums verificationEnums, String uuid);

    /**
     * 预校验
     *
     * @param xPos              位移距离
     * @param uuid              用户唯一表示
     * @param verificationEnums 校验枚举
     * @return
     */
    boolean preCheck(Integer xPos, String uuid, VerificationEnums verificationEnums);

    /**
     * 验证码校验
     *
     * @param uuid              用户唯一表示
     * @param verificationEnums 校验枚举
     * @return 操作结果
     */
    boolean check(String uuid, VerificationEnums verificationEnums);
}
