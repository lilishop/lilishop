package cn.lili.modules.verification.service.impl;

import cn.lili.cache.Cache;
import cn.lili.modules.system.mapper.VerificationSourceMapper;
import cn.lili.modules.verification.entity.dos.VerificationSource;
import cn.lili.modules.verification.entity.dto.VerificationDTO;
import cn.lili.modules.verification.entity.enums.VerificationSourceEnum;
import cn.lili.modules.verification.service.VerificationSourceService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

/**
 * 验证码资源维护 业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 3:48 下午
 */
@Service
public class VerificationSourceServiceImpl extends ServiceImpl<VerificationSourceMapper, VerificationSource> implements VerificationSourceService {

    @Autowired
    private Cache<VerificationDTO> cache;

    @Override
    public VerificationDTO initCache() {
        List<VerificationSource> dbList = this.list();
        List<VerificationSource> resourceList = new ArrayList<>();
        List<VerificationSource> sliderList = new ArrayList<>();
        for (VerificationSource item : dbList) {
            if (item.getType().equals(VerificationSourceEnum.RESOURCE.name())) {
                resourceList.add(item);
            } else if (item.getType().equals(VerificationSourceEnum.SLIDER.name())) {
                sliderList.add(item);
            }
        }
        VerificationDTO verificationDTO = new VerificationDTO();
        verificationDTO.setVerificationResources(resourceList);
        verificationDTO.setVerificationSlider(sliderList);
        cache.put(VERIFICATION_CACHE, verificationDTO);
        return verificationDTO;
    }

    @Override
    public VerificationDTO getVerificationCache() {
        VerificationDTO verificationDTO;
        try {
            verificationDTO = cache.get(VERIFICATION_CACHE);
        } catch (ClassCastException cce) {
            verificationDTO = null;
        }
        if (verificationDTO == null) {
            return initCache();
        }
        return verificationDTO;
    }
}