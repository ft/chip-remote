import makemehappy.manifest as m
import makemehappy.git as git
import makemehappy.utilities as mmh

# mmh.pp(system_instances)

# bd = m.BuildDirectory(
#     build_prefix,
#     'zephyr/nucleo_f302r8/chip-remote/gnuarmemb/debug',
#     logging)
# mmh.pp(bd.cmake())

# m.manifest(m.file('README', root = '..').rename('README.system') > 'doc',
#            m.file('../LICENCE'),
#            m.glob('*.[ch]', root = 'src') ^ 12 > 'sourceCode',
#            m.regex(r'^.*\.txt$', root = 'src') > 'buildSystem')

sd = m.SourceDirectory(git.toplevel('.'))

def chipRemote(prefix, log, vcs):
    def _generate(instance):
        import makemehappy.manifest as m
        (kind, board, app, tc, cfg) = instance.split('/')
        bd = m.BuildDirectory(prefix, instance, log)
        return (m.zephyr(bd, 'chip-remote')
                .filter(m.remove(r'/autoconf\.h$'))
                .transform(m.withVersion(vcs))
                .transform(m.withDashString(cfg))
                .destination(board))
    return _generate

chipRemoteInstances = map(chipRemote(build_prefix, logging, sd.vcs),
                          system_instances)
m.manifest(list(chipRemoteInstances),
           m.file('README')            .rename('README.firmware'),
           m.file('README',  root = sd).rename('README.chip-remote'),
           m.file('LICENCE', root = sd))

# m.manifest(m.file('README', root = sd).filter(m.only('AD')))
