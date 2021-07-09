package cn.lili.modules.system.serviceimpl;

import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.system.entity.dos.InstantDelivery;
import cn.lili.modules.system.entity.plugin.InstantDelivery.InstantDeliveryPlugin;
import cn.lili.modules.system.entity.vo.InstantDeliveryVO;
import cn.lili.modules.system.mapper.InstantDeliveryMapper;
import cn.lili.modules.system.service.InstantDeliveryService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.google.gson.Gson;
import org.elasticsearch.ResourceNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 即时配送业务层实现
 *
 * @author pikachu
 * @date 2020/11/17 8:02 下午
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class InstantDeliveryServiceImpl extends ServiceImpl<InstantDeliveryMapper, InstantDelivery> implements InstantDeliveryService {

    @Autowired
    private List<InstantDeliveryPlugin> instantDeliveryPlugins;

    @Override
    public IPage<InstantDeliveryVO> getInstantDeliveryPage(IPage<InstantDelivery> page, PageVO pageVO) {
        //获取插件和数据库中所有的就是配送方案
        List<InstantDeliveryVO> resultList = this.getInstantDeliveryVOList(page);
        //循环数据，对未入库的数据进行入库操作
        for (InstantDeliveryVO instantDeliveryVO : resultList) {
            //根据id是否为0校验 如果为0则不在数据中，进行入库操作
            if (("0").equals(instantDeliveryVO.getId())) {
                //入库
                InstantDelivery instantDelivery = new InstantDelivery(instantDeliveryVO);
                this.baseMapper.insert(instantDelivery);
            }
        }
        IPage<InstantDeliveryVO> iPage = new Page<>(pageVO.getPageNumber(), pageVO.getPageSize(), resultList.size());
        iPage.setRecords(PageUtil.listToPage(pageVO, resultList));
        return iPage;
    }

    /**
     * 获取即时配送的方案
     *
     * @return 即时配送
     */
    private List<InstantDeliveryVO> getInstantDeliveryVOList(IPage<InstantDelivery> page) {
        //用来构建新的即时配送数据
        List<InstantDeliveryVO> resultList = new ArrayList<>();
        //获取即时配送数据
        List<InstantDelivery> list = page.getRecords();
        Map<String, InstantDelivery> map = new HashMap<>(16);
        for (InstantDelivery instantDelivery : list) {
            map.put(instantDelivery.getDeliveryBean(), instantDelivery);
        }
        //循环检查是否有新的即时配送方式，识别插入数据库
        for (InstantDeliveryPlugin plugin : instantDeliveryPlugins) {
            InstantDelivery instantDelivery = map.get(plugin.getPluginId());
            InstantDeliveryVO result;
            //如果不为空则构建vo参数，否则的话根据插件属性构建vo参数
            if (instantDelivery != null) {
                result = new InstantDeliveryVO(instantDelivery);
            } else {
                result = new InstantDeliveryVO(plugin);
            }
            resultList.add(result);
        }
        return resultList;
    }


    @Override
    public InstantDeliveryVO getInstantDeliveryConfig(String bean) {
        //根据bean获取即时配送方案
        QueryWrapper queryWrapper = new QueryWrapper();
        queryWrapper.eq("delivery_bean", bean);
        InstantDelivery instantDelivery = this.baseMapper.selectOne(queryWrapper);
        if (instantDelivery == null) {
            throw new ResourceNotFoundException("该即时配送方案不存在");
        }
        return new InstantDeliveryVO(instantDelivery);
    }

    @Override
    public void openInstantDelivery(String bean) {
        //关闭所有配送方案
        UpdateWrapper<InstantDelivery> updateWrapper = new UpdateWrapper();
        updateWrapper.set("delivery_open", 0);
        this.update(updateWrapper);
        //开启当前配送方案
        updateWrapper = new UpdateWrapper();
        updateWrapper.eq("delivery_bean", bean);
        updateWrapper.set("delivery_open", 1);
        this.update(updateWrapper);
    }

    @Override
    public InstantDeliveryVO edit(InstantDeliveryVO instantDeliveryVO) {
        //校验此方案是否存在
        this.getInstantDeliveryConfig(instantDeliveryVO.getDeliveryBean());
        //修改即时配送方案
        Gson gson = new Gson();
        UpdateWrapper<InstantDelivery> updateWrapper = new UpdateWrapper<>();
        updateWrapper.set("delivery_config", gson.toJson(instantDeliveryVO.getConfigItems()));
        updateWrapper.eq("delivery_bean", instantDeliveryVO.getDeliveryBean());
        this.update(updateWrapper);
        return instantDeliveryVO;
    }
}