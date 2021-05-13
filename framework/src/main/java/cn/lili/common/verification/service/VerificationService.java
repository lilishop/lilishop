package cn.lili.common.verification.service;

import cn.lili.common.verification.enums.VerificationEnums;

import java.io.IOException;
import java.util.Map;

/**
 * 验证码模块
 */
public interface VerificationService {
    /**
     * 获取校验对象
     *
     * @param verificationEnums 校验枚举
     * @param uuid              uuid
     * @return 校验对象
     */
    Map<String, Object> createVerification(VerificationEnums verificationEnums, String uuid) throws IOException;

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
     * @param uuid              用户唯一表示
     * @param verificationEnums 校验枚举
     * @return
     */
    boolean check(String uuid, VerificationEnums verificationEnums);
}
