package cn.lili.common.verification.service.impl;

import cn.lili.common.cache.Cache;
import cn.lili.common.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.verification.SliderImageUtil;
import cn.lili.common.verification.enums.VerificationEnums;
import cn.lili.common.verification.service.VerificationService;
import cn.lili.common.vo.SerializableStream;
import cn.lili.modules.base.entity.dos.VerificationSource;
import cn.lili.modules.base.entity.vo.VerificationVO;
import cn.lili.modules.base.service.VerificationSourceService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.Random;

/**
 * 认证处理类
 *
 * @author Chopper
 * @version v1.0
 * 2020-11-17 14:59
 */
@Slf4j
@Component
public class VerificationServiceImpl implements VerificationService {

    @Autowired
    private VerificationSourceService verificationSourceService;
    @Autowired
    private Cache cache;

    /**
     * 创建校验
     *
     * @return 验证码参数
     */
    @Override
    public Map<String, Object> createVerification(VerificationEnums verificationEnums, String uuid) throws IOException {

        if (uuid == null) {
            throw new ServiceException(ResultCode.ILLEGAL_REQUEST_ERROR);
        }

        //获取验证码配置
        VerificationVO verificationVO = verificationSourceService.getVerificationCache();

        List<VerificationSource> verificationResources = verificationVO.getVerificationResources();
        List<VerificationSource> verificationSlider = verificationVO.getVerificationSlider();


        Random random = new Random();
        //随机选择需要切的图下标
        int resourceNum = random.nextInt(verificationResources.size());
        //随机选择剪切模版下标
        int sliderNum = random.nextInt(verificationSlider.size());

        //随机选择需要切的图片地址
        String originalResource = verificationResources.get(resourceNum).getResource();
        //随机选择剪切模版图片地址
        String sliderResource = verificationSlider.get(sliderNum).getResource();

        try {
            //获取缓存中的资源
            SerializableStream originalFile = getInputStream(originalResource);
            SerializableStream sliderFile = getInputStream(sliderResource);
            Map<String, Object> resultMap = SliderImageUtil.pictureTemplatesCut(sliderFile, originalFile);
            //生成验证参数 120可以验证 无需手动清除，120秒有效时间自动清除
            cache.put(cacheKey(verificationEnums, uuid), resultMap.get("randomX"), 120L);
            resultMap.put("key", cacheKey(verificationEnums, uuid));
            //移除横坐标移动距离
            resultMap.remove("randomX");
            return resultMap;
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            log.error("创建校验错误",e);
            return null;
        }
    }

    /**
     * 根据网络地址，获取源文件
     * 这里简单说一下，这里是将不可序列化的inputstream序列化对象，存入redis缓存
     * @param originalResource
     * @return
     */
    private SerializableStream getInputStream(String originalResource) throws Exception {

        Object object = cache.get(CachePrefix.VERIFICATION_IMAGE.getPrefix() + originalResource);
        if (object != null) {
            return (SerializableStream) object;
        }
        if (StringUtils.isNotEmpty(originalResource)) {
            URL url = new URL(originalResource);
            InputStream inputStream = url.openStream();
            SerializableStream serializableStream = new SerializableStream(inputStream);
            cache.put(CachePrefix.VERIFICATION_IMAGE.getPrefix() + originalResource, serializableStream);
            return serializableStream;
        }
        return null;
    }

    /**
     * 预校验图片 用于前端回显
     *
     * @param xPos              X轴移动距离
     * @param verificationEnums 验证key
     * @return 验证是否成功
     */
    @Override
    public boolean preCheck(Integer xPos, String uuid, VerificationEnums verificationEnums) {
        Integer randomX = (Integer) cache.get(cacheKey(verificationEnums, uuid));
        if (randomX == null) {
            return false;
        }
        log.debug("{}{}", randomX, xPos);
        //验证结果
        boolean result = Math.abs(randomX - xPos) < 3;
        if (result) {
            //验证成功，则记录验证结果 验证有效时间，120秒
            cache.put(cacheResult(verificationEnums, uuid), true, 120L);
            return result;
        }
        return false;
    }

    /**
     * 验证码校验
     *
     * @param uuid              用户标识
     * @param verificationEnums 验证key
     * @return 验证是否成功
     */
    @Override
    public boolean check(String uuid, VerificationEnums verificationEnums) {
        Object object = cache.get(cacheResult(verificationEnums, uuid));
        if (object == null) {
            return false;
        } else {
            cache.remove(cacheResult(verificationEnums, uuid));
            return true;
        }
    }

    /**
     * 生成缓存key 记录缓存需要验证的内容
     *
     * @param verificationEnums 验证码枚举
     * @param uuid              用户uuid
     * @return 缓存key
     */
    public static String cacheKey(VerificationEnums verificationEnums, String uuid) {
        return CachePrefix.VERIFICATION_KEY.getPrefix() + verificationEnums.name() + uuid;
    }

    /**
     * 生成缓存key 记录缓存验证的结果
     *
     * @param verificationEnums 验证码枚举
     * @param uuid              用户uuid
     * @return 缓存key
     */
    public static String cacheResult(VerificationEnums verificationEnums, String uuid) {
        return CachePrefix.VERIFICATION_RESULT.getPrefix() + verificationEnums.name() + uuid;
    }

}
