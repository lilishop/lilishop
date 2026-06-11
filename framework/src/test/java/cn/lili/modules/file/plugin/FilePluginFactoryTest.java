package cn.lili.modules.file.plugin;

import cn.lili.common.properties.LocalFileProperties;
import cn.lili.modules.file.plugin.impl.LocalFilePlugin;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class FilePluginFactoryTest {

    @TempDir
    Path tempDir;

    @Test
    void filePluginShouldUseLocalStorageWhenOssSettingMissing() throws Exception {
        SettingService settingService = mock(SettingService.class);
        when(settingService.get(SettingEnum.OSS_SETTING.name())).thenReturn(null);

        LocalFileProperties localFileProperties = new LocalFileProperties();
        localFileProperties.setPath(tempDir.toString());
        localFileProperties.setUrlPrefix("/files");

        FilePluginFactory factory = new FilePluginFactory();
        ReflectionTestUtils.setField(factory, "settingService", settingService);
        ReflectionTestUtils.setField(factory, "localFileProperties", localFileProperties);

        FilePlugin plugin = factory.filePlugin();
        String url = plugin.inputStreamUpload(
                new ByteArrayInputStream("default".getBytes(StandardCharsets.UTF_8)),
                "default/a.txt",
                "text/plain"
        );

        assertInstanceOf(LocalFilePlugin.class, plugin);
        assertEquals("/files/default/a.txt", url);
        assertEquals("default", Files.readString(tempDir.resolve("default/a.txt")));
    }
}
